% ============================================================================
% CONSTRAINT STORY: pension_liability_spiraling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pension_liability_spiraling, []).

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
 *   constraint_id: pension_liability_spiraling
 *   human_readable: Pension Liability Spiraling in Public and Private Systems
 *   domain: fiscal_policy/labor_economics/institutional_finance
 *
 * SUMMARY:
 *   Pension liability spiraling represents a structural entrapment where
 *   legacy benefit promises made at favorable worker-to-retiree ratios become
 *   unsustainable as demographics shift. The constraint exhibits features of
 *   pure extraction (younger workers trapped by mandatory contributions to
 *   pay current retirees), pure coordination (pension systems solve
 *   legitimate retirement security problems), and institutional theater
 *   (actuarial valuations delay insolvency recognition through assumption
 *   optimization). The spiraling dynamic occurs because political barriers
 *   prevent simultaneous adjustment of all three parameters — benefits,
 *   contributions, and retirement age — forcing the burden onto those with
 *   least political power: future workers and younger cohorts. The constraint
 *   operates across multiple temporal scales: immediate (current benefit
 *   payments require current contributions), biographical (mid-career workers
 *   face vesting lock-in), and generational (future taxpayers inherit
 *   obligation for promises made decades prior). Theater_ratio increases over
 *   the 30-year interval as the gap between reported actuarial solvency and
 *   structural reality widens, driven by increasingly optimistic return
 *   assumptions, mortality improvements, and wage growth forecasts.
 *
 * KEY AGENTS:
 *   - Current Pension Recipients (Institutional/Arbitrage) — Primary beneficiaries; experience the constraint as coordination mechanism for deferred compensation
 *   - Younger Workers and Future Cohorts (Powerless/Trapped) — Primary victims; face mandatory contributions with reduced expected benefits due to demographic shift
 *   - Future Taxpayers (Powerless/Trapped) — Secondary victims; will bear general-fund taxation needed to pay unfunded obligations
 *   - Financial Intermediaries (Institutional/Arbitrage) — Secondary beneficiaries; extract fees on pension asset management
 *   - Government Pension Administration (Institutional/Constrained) — Manages the constraint through actuarial theater; identity fused with system defense
 *   - Organized Political Coalitions (Organized/Constrained) — Unions and pensioner organizations block adjustment; create coordination gridlock
 *   - Analytical Observer (Analytical/Analytical) — Risks naturalizing policy choices as demographic immutability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pension_liability_spiraling, 0.58).
domain_priors:suppression_score(pension_liability_spiraling, 0.62).
domain_priors:theater_ratio(pension_liability_spiraling, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pension_liability_spiraling, extractiveness, 0.58).
narrative_ontology:constraint_metric(pension_liability_spiraling, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(pension_liability_spiraling, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pension_liability_spiraling, tangled_rope).
narrative_ontology:human_readable(pension_liability_spiraling, "Pension Liability Spiraling in Public and Private Systems").
narrative_ontology:topic_domain(pension_liability_spiraling, "fiscal_policy/labor_economics/institutional_finance").

domain_priors:requires_active_enforcement(pension_liability_spiraling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pension_liability_spiraling, current_pension_recipients).
narrative_ontology:constraint_beneficiary(pension_liability_spiraling, financial_intermediaries).
narrative_ontology:constraint_victim(pension_liability_spiraling, future_workers).
narrative_ontology:constraint_victim(pension_liability_spiraling, taxpayers).
narrative_ontology:constraint_victim(pension_liability_spiraling, younger_cohorts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNGER WORKER (SNARE) — Trapped by mandatory contribution requirements and unfavorable benefit formulas. Exit options are near-zero: cannot avoid contributions, cannot migrate to alternative retirement systems, faces deteriorating benefit-to-contribution ratios. The extraction is total and unavoidable — the younger worker bears the cost of legacy benefit promises while receiving diminished returns.
constraint_indexing:constraint_classification(pension_liability_spiraling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE TAXPAYER (SNARE) — Trapped across a generational horizon. Government pension liabilities are increasingly funded through general taxation rather than dedicated contributions. Future taxpayers will bear the cost of promises made to previous generations. No exit option exists short of migration out of the taxing jurisdiction — and migration itself is costly and constrains other life options.
constraint_indexing:constraint_classification(pension_liability_spiraling, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CURRENT PENSION RECIPIENTS / UNIONS (ROPE) — Primary beneficiaries. Experience the constraint as pure coordination: pension systems coordinate deferred compensation and retirement security for workers. Net benefit flows to this group. Arbitrage options available: union bargaining power, political organization, pension fund governance participation. The constraint functions as collective benefit provision from their structural position.
constraint_indexing:constraint_classification(pension_liability_spiraling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MID-CAREER PUBLIC SECTOR EMPLOYEE (TANGLED ROPE) — Constrained by vesting schedules and pension formula lock-in. Benefits from defined-benefit promise (coordination function) but bears asymmetric risk if the system becomes insolvent or benefits are cut. Cannot exit mid-career without significant penalty. Faces both coordination benefit and extraction cost — mixed position within the constraint.
constraint_indexing:constraint_classification(pension_liability_spiraling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FINANCIAL INTERMEDIARIES (ROPE) — Benefit from management of massive pension fund assets (trillions globally). Earn fees on asset management, enjoy negotiating power with public trustees, and benefit from asset concentration. Experience the constraint as coordination mechanism that generates recurring revenue. Arbitrage options: can exit as fund manager, can influence asset allocation policies. Low suppression from this perspective — the mechanism serves their interests.
constraint_indexing:constraint_classification(pension_liability_spiraling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GOVERNMENT PENSION ADMINISTRATION (PITON) — Maintains actuarial pretense (theater_ratio 0.68) through accounting conventions, assumption optimism, and periodic revaluation cycles that rarely trigger benefit cuts. The primary function (coordinating retirement security) has atrophied into management of legacy liabilities. Organizational identity is fused with defending the system rather than reforming it. Theater persists: actuarial valuations use strategic assumptions (long investment return forecasts, mortality improvements, wage growth projections) that delay rather than resolve insolvency.
constraint_indexing:constraint_classification(pension_liability_spiraling, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ORGANIZED POLITICAL COALITIONS (TANGLED ROPE) — Unions, pensioner organizations, and sympathetic legislators can mobilize to block benefit cuts or tax increases. This organized power creates a coordination problem: all parties benefit from sustainable systems but none can unilaterally impose losses. The constraint both coordinates retirement provision (genuine function) and prevents any actor from bearing full adjustment cost (extraction mechanism). Suppression moderate — organizations have voice but constrained by fiscal reality.
constraint_indexing:constraint_classification(pension_liability_spiraling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / DEMOGRAPHIC IMMUTABILITY VIEW (MOUNTAIN) — From a civilizational view, pension liabilities are immutable consequences of demographic transition: aging populations have higher retiree-to-worker ratios, and defined-benefit promises made at higher ratios become mathematically impossible at lower ratios. This is a natural law of demography, not a contingent institutional choice. However, the structural data suggests this is a false summit: many nations with identical demographic profiles maintain sustainable systems through different benefit formulas, contribution rates, and retirement ages. The 'natural law' framing naturalizes policy choices.
constraint_indexing:constraint_classification(pension_liability_spiraling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pension_liability_spiraling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pension_liability_spiraling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pension_liability_spiraling, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pension_liability_spiraling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pension_liability_spiraling, TR),
    TR >= 0.70.

:- end_tests(pension_liability_spiraling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts significantly from younger workers and future taxpayers through unfavorable contribution-benefit ratios. The extraction is not total (many systems remain solvent or near-solvent, and workers do receive eventual benefits) but is substantial and growing. Mid-career workers benefit from defined-benefit promises but bear asymmetric risk of cuts or tax increases. Suppression (0.62): Multiple barriers prevent exit or alternative provision: mandatory contributions, vesting schedules that penalize mid-career movement, legal prohibitions on opt-out, and tax treatment that favors qualified pensions. Information barriers are high — most workers do not understand the actuarial math of their liability position. Coordination barriers prevent coalition-building among younger workers (geographically dispersed, temporally distant from retirement). Theater ratio (0.68): Significant performative content. Actuarial valuations routinely use optimistic assumptions about investment returns (historically 7-8% when market returns average 5-6%), mortality improvements, and wage growth. Periodic revaluations cycle through assumption tweaks rather than structural reform. Political theater: debate frames the constraint as demographic inevitability rather than policy choice, naturalizing legacy promises that could be adjusted.
 *
 * PERSPECTIVAL GAP:
 *   Why do current recipients and younger workers disagree so sharply? Current recipients experience the constraint from the institutional side — they solved the problem of insecure retirement by creating a collective system. For them, the constraint is a coordination success. Younger workers experience it from the victim side — they bear mandatory extraction to pay benefits they are unlikely to receive at equivalent replacement rates. The gap is real and structural: it reflects different positions in the extraction flow. The system coordinates retirement provision (genuine function) while extracting from younger cohorts to pay for past generosity (extraction function). Both are simultaneously true. The divergence reveals that the constraint is hybrid — tangled_rope — not pure coordination or pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is computed from three structural facts: (1) who benefits (current recipients, financial intermediaries), (2) who bears costs (younger workers, future taxpayers, mid-career constrained workers), and (3) what exit options each faces (arbitrage for beneficiaries, trapped for victims). The beneficiary groups have low d (resources, political power, exit options: 0.05-0.20). The victim groups have high d (no resources, no political power, no exit: 0.85-0.98). The constrained midpoint groups have moderate d (0.50-0.65). The sigmoid amplifies the experienced burden on high-d agents and subsidizes the benefit for low-d agents. No directionality override is needed; the structural data is clear and unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that both coordination and extraction functions are real and structural. The system genuinely coordinates retirement provision — it solves a legitimate collective action problem (individual retirement savings is hard; pooling reduces risk). But it simultaneously extracts from younger cohorts to fund excessive benefits promised to earlier cohorts. The resolution is not 'which function is real?' but 'how are they layered?' The constraint must be classified as tangled_rope, not rope, because suppression is high (0.62), beneficiaries are identifiable (current recipients, intermediaries), victims are identifiable (younger workers), and the system requires active enforcement (mandatory contributions, vesting locks, legal prohibition on withdrawal). A pure-rope classification would obscure the extraction function. A pure-snare classification would obscure the genuine coordination benefit. Tangled_rope captures both: legitimate collective problem-solving layered with significant asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_vs_policy_attribution,
    'Is pension insolvency driven by demographics (aging) or by policy choices (benefit formulas, contribution rates, investment assumptions)?',
    'Comparative analysis of nations with similar demographic profiles but different pension sustainability outcomes. Counterfactual modeling: what benefit formula or contribution rate would generate system sustainability at observed demographics?',
    'If primarily demographic: mountain classification strengthened; reform capacity is limited to adjusting parameters to match demographics. If primarily policy: false summit; constraint is a tangled_rope of legacy promises and political blocking, not an immutable law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_vs_policy_attribution, empirical, 'Attribution of insolvency to demographic inevitability vs policy choice').

omega_variable(
    intergenerational_extraction_symmetry,
    'Do younger workers perceive the extraction asymmetrically because they have less political power, or because the system genuinely structures extraction toward them independent of political leverage?',
    'Historical comparison: do past demographic transitions (lower birth rates emerging) that increase worker burden correlate with policy adjustments that protect workers, or with benefit maintenance that shifts burden? Do political-economy models predict younger worker extraction even if their power increased?',
    'If power-dependent: reform through coalition-building can rebalance. If structurally extraction-first: the constraint is snare-like even if organized power emerges, because the structure prefers extracting from the politically weak.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_extraction_symmetry, empirical, 'Whether extraction is structural or political-contingent').

omega_variable(
    actuarial_assumption_strategic_bias,
    'Are pension fund actuarial assumptions (return forecasts, mortality improvements, wage growth) systematically optimistic in ways that delay insolvency recognition, or do they reflect genuine uncertainty ranges?',
    'Retrospective analysis: compare assumptions used in valuations 5/10/15 years ago with actual realizations. Measure frequency of assumption revision upward vs downward. Compare distribution of realized returns to forecasted return assumptions.',
    'If systematically biased: theater_ratio is understated; the performative component is higher than reported. Constraint is more piton-like (inertial) than tangled_rope. If genuinely uncertain: current theater_ratio (0.68) is appropriate; uncertainty is real, not purely theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actuarial_assumption_strategic_bias, empirical, 'Strategic bias in actuarial assumption-setting').

omega_variable(
    benefit_cut_political_feasibility,
    'Are current benefit levels truly immune from reduction (trapped workers), or are they politically difficult but structurally feasible to adjust (constrained)?',
    'Examine historical cases: which nations have successfully adjusted public pension benefits? What political conditions enabled adjustment? What barriers emerged? Model voter preference distributions and coalition payoffs under benefit-cut scenarios.',
    'If truly impossible (mountain): the constraint is immutable and spiraling is irreversible short of massive migration/tax evasion. If difficult but possible (tangled_rope): reform requires coalition-building, but exit paths exist. This affects victim exit_options classification (trapped vs constrained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(benefit_cut_political_feasibility, empirical, 'Political feasibility of pension benefit adjustment').

omega_variable(
    financial_intermediary_rent_extraction,
    'Do financial intermediaries amplify pension insolvency by extracting fees that reduce returns, or do they provide genuine value-add that enables returns matching actuarial assumptions?',
    'Compare net returns (gross returns minus fees) achieved by pension funds to index returns on equivalent asset classes. Measure fee-drag over 10+ year periods. Compare returns across fund size and management model (in-house vs external management).',
    'If rent-extracting: financial intermediaries are a secondary extraction mechanism layered onto the primary younger-worker extraction. System spirals faster due to fee drag. If value-adding: intermediaries are genuine beneficiaries earning coordination rewards, and the primary extraction mechanism is the benefit-contribution asymmetry alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financial_intermediary_rent_extraction, empirical, 'Financial intermediary impact on pension sustainability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pension_liability_spiraling, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pens_tr_t0, pension_liability_spiraling, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pens_tr_t10, pension_liability_spiraling, theater_ratio, 10, 0.58).
narrative_ontology:measurement(pens_tr_t20, pension_liability_spiraling, theater_ratio, 20, 0.68).
narrative_ontology:measurement(pens_tr_t30, pension_liability_spiraling, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(pens_be_t0, pension_liability_spiraling, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pens_be_t10, pension_liability_spiraling, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(pens_be_t20, pension_liability_spiraling, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(pens_be_t30, pension_liability_spiraling, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pension_liability_spiraling, resource_allocation).
narrative_ontology:affects_constraint(pension_liability_spiraling, fiscal_capacity_constraint).
narrative_ontology:affects_constraint(pension_liability_spiraling, intergenerational_wealth_transfer).
narrative_ontology:affects_constraint(pension_liability_spiraling, labor_market_participation_dynamics).

% DUAL FORMULATION NOTE:
% Pension liability spiraling is downstream of demographic transition and policy choices made decades prior. It affects fiscal capacity of governments (crowding out public investment), intergenerational wealth transfers (implicit tax on younger cohorts), and labor market dynamics (retirement age increases, younger worker wage suppression to fund contribution increases).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pension_liability_spiraling, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
