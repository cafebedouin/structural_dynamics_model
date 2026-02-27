% ============================================================================
% CONSTRAINT STORY: usc_26_469_passive_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usc_26_469_passive_loss, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usc_26_469_passive_loss
 *   human_readable: Passive Activity Loss (PAL) Rules (IRC Section 469)
 *   domain: economic/legal/tax_policy
 *
 * SUMMARY:
 *   The Passive Activity Loss rules (IRC Section 469), enacted in the Tax
 *   Reform Act of 1986, prohibit taxpayers from using losses from passive
 *   activities (rental real estate, partnership interests without material
 *   participation) to offset active income (W-2 wages) or portfolio income
 *   (dividends, interest). The rule emerged as a response to the pre-1986 tax
 *   shelter industry, which generated billions in uncollected revenue through
 *   loss trafficking schemes. However, over 30+ years, the rule has
 *   accumulated exceptions, safe harbors, and compliance theater while
 *   extracting meaningful deductions from middle-class real estate investors
 *   and small developers. The constraint exhibits a perspectival gap: the
 *   U.S. Treasury sees a revenue-protection mechanism (Rope); small landlords
 *   see a permanent extraction trap (Snare); large real estate corporations
 *   see both a coordination standard and a planning opportunity (Tangled
 *   Rope); tax professionals see a compliance procedural drag (Piton); reform
 *   advocates see a temporary expedient awaiting tax code simplification
 *   (Scaffold). The theater ratio has risen from 0.35 (simple loss denial in
 *   early years) to 0.58 (complex material participation tests, real estate
 *   professional determinations, qualified business income planning) as
 *   compliance infrastructure has expanded without corresponding
 *   simplification of the underlying economic principle.
 *
 * KEY AGENTS:
 *   - U.S. Treasury/IRS: Primary beneficiary (institutional/arbitrage) — prevents revenue loss from loss trafficking; can adjust enforcement intensity; captures $8-15 billion annually in forgone deductions
 *   - Small Real Estate Investors: Primary victim (powerless/trapped) — own rental property with legitimate losses (depreciation, maintenance, vacancy) but cannot deduct against W-2 wages; trapped by income phase-out ($25,000 threshold declining with MAGI)
 *   - Real Estate Industry Associations: Secondary victim/actor (organized/constrained) — lobby for exceptions but politically constrained; benefit from coordination function (standardized definitions)
 *   - Large Real Estate Corporations and Syndicators: Secondary beneficiary (powerful/arbitrage) — can structure around PAL (QBI pass-through, real estate professional status, cost segregation); effective arbitrage reduces experienced extraction
 *   - Tax Compliance Professionals: Piton beneficiary (institutional/constrained) — maintain expertise domain in passive loss planning; benefit from rule complexity; see own work as performative
 *   - Tax Reform Coalition: Organized agent (organized/constrained) — perceives sunset path through capital gains reform or wealth taxation; views PAL as temporary scaffold within broader tax simplification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usc_26_469_passive_loss, 0.48).
domain_priors:suppression_score(usc_26_469_passive_loss, 0.62).
domain_priors:theater_ratio(usc_26_469_passive_loss, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usc_26_469_passive_loss, extractiveness, 0.48).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(usc_26_469_passive_loss, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usc_26_469_passive_loss, tangled_rope).
narrative_ontology:human_readable(usc_26_469_passive_loss, "Passive Activity Loss (PAL) Rules (IRC Section 469)").
narrative_ontology:topic_domain(usc_26_469_passive_loss, "economic/legal/tax_policy").

domain_priors:requires_active_enforcement(usc_26_469_passive_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, us_treasury).
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, active_income_earners).
narrative_ontology:constraint_beneficiary(usc_26_469_passive_loss, wage_dependent_households).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, real_estate_investors).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, passive_business_owners).
narrative_ontology:constraint_victim(usc_26_469_passive_loss, small_real_estate_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL REAL ESTATE INVESTOR (SNARE) — Owns rental property with material losses but cannot deduct them against W-2 wages or active business income. Trapped by the passive loss deduction ceiling ($25,000 for certain taxpayers, declining with income). No meaningful exit: the property cannot be easily liquidated without triggering capital gains, and alternative investments may not generate the same community benefit or inflation hedge. Maximum experienced extraction through loss carryforward suspension.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REAL ESTATE INDUSTRY ASSOCIATIONS (TANGLED ROPE) — Collectively organized to lobby for PAL exceptions and safe harbors (e.g., real estate professional status, qualified business income deduction under TCJA 2017). Benefits from coordination on technical tax planning and exception design. Constrained by political exposure: advocating for loss deductions appears to defend 'tax loopholes.' Experiences both asymmetric extraction (loss restrictions) and genuine coordination function (standardizing which activities count as 'material participation').
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. TREASURY AND IRS (ROPE) — Primary beneficiary. PAL rules prevent revenue loss from aggressive passive loss shelters (pre-1986 tax shelters cost billions in uncollected revenue). Experiences the constraint as a pure coordination mechanism: defining the boundary between passive and active income solves the collective action problem of preventing widespread loss trafficking. The treasury has high arbitrage: if PAL enforcement weakens, it can adjust audit intensity or other provisions. Net beneficiary—extraction flows toward this institutional actor.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE REAL ESTATE CORPORATIONS AND SYNDICATORS (TANGLED ROPE) — Powerful actors with resources to structure around PAL rules (qualified business income pass-through treatment, real estate professional status planning, cost segregation). Experiences constraint as a coordination mechanism (standardized definitions enable large-scale syndication) but also asymmetric extraction (smaller competitors cannot afford sophisticated planning). Arbitrage exit available: shift to active real estate development, achieve qualified business income status, or hold in C-corporate structures. Effective extraction is moderate because power enables exit design.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PASSIVE LOSS SHELTER COMPLIANCE INDUSTRY (PITON) — Tax planners, CPA firms, and legal advisors who maintain the PAL definitions and exceptions as a professional domain. Theater ratio is high: much compliance activity (determining 'material participation,' tracking passive loss carryforwards, qualifying for real estate professional status) is procedurally complex but economically inert—it creates no real value, only ensures compliance with the rule's boundary conditions. The industry benefits from the rule's complexity but sees its own work as largely performative. Institutional inertia maintains the rule despite ongoing political pressure for simplification.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TAX REFORM COALITION (SCAFFOLD) — Progressive coalitions advocating for capital gains taxation reform, wealth taxes, or simplified loss limitations see PAL as a temporary scaffolding supporting broader tax fairness goals. If wealth taxation is enacted, the entire passive loss regime may be superseded by a simpler mark-to-market system. Sunset clause is implicit in the legislative horizon: the 2017 Tax Cuts and Jobs Act's qualified business income deduction (expires 2025) represents a partial sunset of PAL's scope for pass-through entities. Low effective extraction because organized agents perceive a 10-15 year exit path through comprehensive tax reform.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational economic perspective, loss limitation is an inherent feature of any tax system: if losses could offset all income indefinitely, wealth could be indefinitely transferred through loss trafficking without tax cost. This perspective sees PAL as an immutable consequence of taxation design, equivalent to depreciation recapture or the capital gains lockup effect. However, the structural data contradicts the mountain classification—the rule is a 1986 legislative choice with specific exceptions and exemptions, making it a contingent institutional arrangement rather than a natural law.
constraint_indexing:constraint_classification(usc_26_469_passive_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usc_26_469_passive_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usc_26_469_passive_loss, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(usc_26_469_passive_loss, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(usc_26_469_passive_loss, TR),
    TR >= 0.70.

:- end_tests(usc_26_469_passive_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, declining. Initial extractiveness was 0.68 (1986-1990) when the rule was a pure denial—losses were permanently suspended and many taxpayers had no realistic path to passive income to utilize carry-forwards. The 2017 qualified business income deduction and expanded real estate professional exceptions reduced extractiveness to 0.48 by allowing many syndication structures and active real estate businesses to bypass PAL entirely. The rule still extracts meaningful revenue (estimated $8-15B annually), but accommodations have reduced the net effect. Suppression (0.62): Moderate-high. Significant barriers to exit include: (1) capital gains triggered by property sales, (2) illiquidity of real estate, (3) information asymmetries in syndication structures, (4) professional/business reputation constraints on exit (selling a family rental generates social friction), (5) material participation determination complexity. However, suppression is not absolute—exit options exist for high-income taxpayers (real estate professional status, C-corporation restructuring) and for low-income taxpayers (losses are smaller in absolute value, often fully utilized within 5-7 years of property operation). Theater ratio (0.58): Moderate-high and rising. The material participation tests (Section 469(h), 590 hours or more) are procedurally complex and economically inert—they create extensive documentation requirements without corresponding economic meaning. The real estate professional status exception has similarly high theater: taxpayers must track professional hours, file Form 8582, manage passive loss carryforwards across multiple tax years. Much of this compliance activity is formal without substance—if the rule were eliminated, the economic outcomes would be identical, only the tax outcomes would change.
 *
 * PERSPECTIVAL GAP:
 *   Small real estate investors see the PAL rule as an irreversible extraction (Snare): their losses vanish if not offset within the biographical horizon, and exit from real estate ownership triggers capital gains tax. The Treasury sees pure coordination (Rope): the rule prevents a clear collective action problem (loss trafficking). Large corporations see mixed coordination and extraction (Tangled Rope): the rule's definitions (material participation, real estate professional status) enable large-scale syndication, but exceptions are unequally distributed by wealth/sophistication. Tax professionals see performative compliance (Piton): material participation tests and passive loss carryforward tracking generate extensive procedure with minimal economic meaning. Reform advocates see a temporary expedient (Scaffold): wealth taxation or mark-to-market systems could replace PAL with simpler rules. The analytical observer risks seeing a natural law (Mountain): loss limitations are inherent to any tax system, making PAL appear immutable. However, the structural data contradicts this—PAL is a 1986 legislative choice with specific exceptions that have repeatedly been revised (1989, 1992, 2017), indicating that the 'natural' boundary is actually a moving political line.
 *
 * DIRECTIONALITY LOGIC:
 *   Small real estate investors occupy the victim position: they are trapped (capital gains on sale, illiquidity, material participation documentation barriers), have low power (insufficient resources for tax planning), and face a long biographical horizon to utilize carry-forwards. This produces high d (~0.90) and high f(d) (~1.40), creating high chi despite moderate base extractiveness. The Treasury occupies the beneficiary position: it has high power (institutional enforcement authority), arbitrage exit (can adjust audit intensity, legislative lobbying), and a short time horizon (immediate revenue impact). This produces low d (~0.10) and negative f(d) (~-0.10), creating negative chi (the rule subsidizes this agent). Large corporations occupy an intermediate position: they are victims of the rule's form (loss restrictions) but beneficiaries of exceptions (QBI deduction, real estate professional status), producing moderate d (~0.45) and f(d) (~0.55), middle-range chi. The analytical observer's d is derived from exit options (analytical) and power (analytical), producing d ~0.73 and f(d) ~1.15—moderate experienced extraction in the abstract sense of observing the constraint's structural features.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY (0.48 extractiveness, below 0.70 threshold for mandatory resolution). The mandatrophy question: Is PAL a necessary coordination mechanism (distinguishing legitimate passive investment from aggressive loss trafficking) or an extractive shelter that has spawned a compliance industry of exceptions and safe harbors? The evidence supports both: (1) The pre-1986 tax shelter collapse produced real economic costs (billions in uncollected revenue), suggesting PAL solved a genuine coordination problem; (2) The 30-year accumulation of exceptions (real estate professional, QBI deduction, passive loss carryforwards, syndication structures) suggests the rule has been captured by tax planning interests, making it increasingly extractive and theatrical. The perspectival gap between the Treasury (sees Rope) and small investors (see Snare) is unresolved. If empirical omega 'real_estate_professional_abuse' shows extensive loopholes (high abuse), the classification should upgrade to Snare or Piton. If it shows legitimate carve-outs with low abuse, the classification should downgrade to Rope. Current data is inconclusive; the rule remains tangled rope until one of the omegas is resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_participation_boundary,
    'Is the line between ''passive'' and ''active'' material participation a natural boundary or a political choice amenable to redefinition?',
    'Historical analysis of the evolving material participation tests (Section 469(h) regulations changed 1989, 1992, 2008); comparison with international passive loss regimes (EU, Canada) and their boundaries; econometric analysis of whether the 500-hour threshold (current standard) produces different outcomes at 450 or 550 hours',
    'If natural/stable: PAL is closer to mountain. If political/revisable: PAL is clearly tangled rope. If boundary is arbitrary: the rule becomes piton (compliance theater with no economic principle).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_participation_boundary, conceptual, 'Whether material participation boundary is natural or politically contingent').

omega_variable(
    real_estate_professional_abuse,
    'To what extent is the ''real estate professional'' exception (Section 469(c)(7)) a legitimate carve-out for genuine practitioners vs. a loophole exploited by high-income taxpayers to offset passive losses against wages?',
    'Empirical audit data on real estate professional status claims; IRS enforcement patterns (examination rates, disallowance rates); survey of practitioners'' time allocation to management vs. brokerage/development; analysis of income concentration among successful real estate professionals claiming the exception',
    'If primarily legitimate: PAL system is functioning as designed (rope for beneficiaries, tangled rope for constrained actors). If primarily abused: PAL is snare (losing revenue through exceptions). If unclear: omega remains high and mandatrophy is unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(real_estate_professional_abuse, empirical, 'Extent of real estate professional exception abuse').

omega_variable(
    passive_loss_carryforward_utilization,
    'What fraction of suspended passive losses are ultimately utilized (deducted against future passive income or death-triggered release) vs. lost permanently through taxpayer exit (sale, death without offsetting passive income, abandonment)?',
    'Longitudinal IRS data tracking passive loss carryforwards from origination to utilization or expiration; cohort analysis of taxpayers by entry year and loss suspension amount; cross-reference with estate tax records and property disposition records',
    'If > 70% utilized: PAL is a timing rule (rope-adjacent coordination). If < 30% utilized: PAL is permanent extraction (snare). If 30-70% utilized: tangled rope classification is confirmed—some losses are eventually used, but many are never deducted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(passive_loss_carryforward_utilization, empirical, 'Utilization rate of suspended passive losses over time').

omega_variable(
    behavioral_response_to_pal,
    'Has PAL changed investment patterns toward higher-income portfolios or away from real estate investment for moderate-income taxpayers? Or have accommodations (passive loss carryforward stacking, real estate professional status, QBI deduction) rendered the constraint ineffective?',
    'Time-series analysis of real estate investment rates (rental property ownership, passive business partnerships) before/after 1986 across income cohorts; control for interest rates, property appreciation, and alternative investment returns; analysis of QBI deduction take-up rates and interaction with passive loss rules',
    'If strong behavioral response (reduced real estate investment): PAL is effective extraction (snare from small investor view). If weak/zero response (accommodations fully offset): PAL is mostly piton (theater, no real constraint). If heterogeneous response (affects small investors but not large): PAL is snare for the poor, rope for the powerful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_response_to_pal, empirical, 'Behavioral response to PAL rules across income cohorts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usc_26_469_passive_loss, 1986, 2016).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pal_tr_t0, usc_26_469_passive_loss, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pal_tr_t15, usc_26_469_passive_loss, theater_ratio, 15, 0.48).
narrative_ontology:measurement(pal_tr_t30, usc_26_469_passive_loss, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(pal_be_t0, usc_26_469_passive_loss, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(pal_be_t15, usc_26_469_passive_loss, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(pal_be_t30, usc_26_469_passive_loss, base_extractiveness, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usc_26_469_passive_loss, enforcement_mechanism).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, qualified_business_income_deduction).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, depreciation_recapture).
narrative_ontology:affects_constraint(usc_26_469_passive_loss, real_estate_syndication_structure).

% DUAL FORMULATION NOTE:
% The PAL rule decomposes into two structurally distinct constraints: (1) Loss limitation as revenue protection (low extractiveness, ~0.15, Mountain/Rope) preventing pre-1986 tax shelter collapse—this is the original 1986 intent; (2) Loss limitation as active extraction (extractiveness ~0.55-0.68) targeting small real estate investors who fall through exception carve-outs—this is the observed effect in 2010+. These are separate constraints linked through the exceptions regime. The first would be Mountain if measured in isolation (immutable coordination function); the second is Tangled Rope/Snare when measured at the small-investor level. The network edge indicates that QBI deduction, real estate professional status determinations, and depreciation recapture rules all modulate the effective extractiveness of the base PAL rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usc_26_469_passive_loss, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
