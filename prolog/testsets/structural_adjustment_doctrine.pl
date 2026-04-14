% ============================================================================
% CONSTRAINT STORY: structural_adjustment_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_structural_adjustment_doctrine, []).

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
 *   constraint_id: structural_adjustment_doctrine
 *   human_readable: Structural Adjustment Doctrine in Development Finance
 *   domain: economic_policy/international_development
 *
 * SUMMARY:
 *   Structural Adjustment Programs (SAPs) represent a 50-year global
 *   constraint imposed through IMF and World Bank conditionality on debtor
 *   nations. The doctrine mandates fiscal austerity, currency devaluation,
 *   privatization of public services, trade liberalization, and removal of
 *   capital controls as conditions for debt rescheduling and new financing.
 *   The constraint exhibits dramatically different classifications depending
 *   on the observer's structural position. Creditors and international
 *   financial institutions perceive coordination (rope): SAPs solve a genuine
 *   problem of sovereign debt management and moral hazard. Debtor nation
 *   citizens, trapped by external debt obligations and unable to exit,
 *   perceive pure extraction (snare): they bear costs (job loss, reduced
 *   healthcare/education, currency collapse) without negotiation or consent.
 *   The doctrine is justified through neoclassical economic theory that has
 *   deteriorated empirically over decades but persists through institutional
 *   inertia (piton). Alternative policy frameworks exist (Keynesian stimulus,
 *   sectoral development, heterodox fiscal policy) but are suppressed through
 *   institutional conditionality and ideological capture. The theater ratio
 *   reflects the gap between SAP rhetoric ('growth-oriented structural
 *   reform') and institutional reality ('creditor protection mechanism
 *   maintained through donor coordination'). Measurement data shows
 *   theater_ratio rising from 0.35 (1980s, when SAPs claimed measurable
 *   growth benefits) to 0.58 (2020s, when growth outcomes are equivocal but
 *   institutional mechanisms persist). Extractiveness rises from 0.52 to 0.68
 *   as conditionality becomes more intrusive and citizens' exit options
 *   narrow.
 *
 * KEY AGENTS:
 *   - IMF/World Bank Consortium: Institutional beneficiary (institutional/arbitrage) — architects and enforcers of SAP conditionality; capture rents from loan disbursement and program design fees
 *   - Foreign Creditor Coalition: Institutional beneficiary (institutional/arbitrage) — bondholders and commercial bank syndicate; primary beneficiaries of debt-service enforcement
 *   - Debtor Nation Citizens: Primary victim (powerless/trapped) — bear job losses, social service reductions, currency devaluation; trapped by sovereign debt obligations and inability to exit
 *   - Domestic Industry Sectors: Secondary victim (moderate/constrained) — unprotected from import competition; constrained by tariff removal and exchange rate volatility
 *   - Reformist Government Technocrats: Mixed actor (institutional/constrained) — benefit from World Bank funding and career advancement; constrained by domestic legitimacy pressure and international conditions
 *   - Labor and Civil Society Organizations: Organized opposition (organized/constrained) — can delay/modify SAP implementation through collective action; suppressed through criminalization and organizational barriers
 *   - Neoclassical Economics Consensus: Institutional knowledge structure (analytical/analytical) — theoretical justification for SAP logic; persists despite empirical degradation (piton mechanism)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(structural_adjustment_doctrine, 0.68).
domain_priors:suppression_score(structural_adjustment_doctrine, 0.72).
domain_priors:theater_ratio(structural_adjustment_doctrine, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(structural_adjustment_doctrine, extractiveness, 0.68).
narrative_ontology:constraint_metric(structural_adjustment_doctrine, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(structural_adjustment_doctrine, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(structural_adjustment_doctrine, snare).
narrative_ontology:human_readable(structural_adjustment_doctrine, "Structural Adjustment Doctrine in Development Finance").
narrative_ontology:topic_domain(structural_adjustment_doctrine, "economic_policy/international_development").

domain_priors:requires_active_enforcement(structural_adjustment_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(structural_adjustment_doctrine, imf_world_bank).
narrative_ontology:constraint_beneficiary(structural_adjustment_doctrine, multinational_corporations).
narrative_ontology:constraint_beneficiary(structural_adjustment_doctrine, foreign_creditors).
narrative_ontology:constraint_victim(structural_adjustment_doctrine, recipient_nation_populations).
narrative_ontology:constraint_victim(structural_adjustment_doctrine, domestic_industry_sectors).
narrative_ontology:constraint_victim(structural_adjustment_doctrine, public_service_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RECIPIENT NATION POPULATIONS (SNARE) — Citizens of debtor nations face privatization of public services, currency devaluation, austerity in healthcare and education, and job losses in protected sectors. Trapped by sovereign debt obligations and IMF conditionality; no exit options short of default (which carries severe penalties). Maximum experienced extraction — bearing full cost of structural adjustment without consent or exit.
constraint_indexing:constraint_classification(structural_adjustment_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DOMESTIC INDUSTRY SECTORS (SNARE) — Import liberalization forces unprotected domestic industries to compete with multinational corporations; tariff removal eliminates price protection. Workers face wage suppression and unemployment. Constrained exits: skill-based emigration available to some workers, but most lack resources or credentials. Extraction concentrated in trade-affected sectors.
constraint_indexing:constraint_classification(structural_adjustment_doctrine, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: IMF/WORLD BANK & CREDITOR COALITION (ROPE) — Beneficiaries perceive structural adjustment as solving a genuine coordination problem: how to allocate credit and enforce fiscal discipline across sovereign debtors. The mechanism coordinates debt repayment with domestic policy reform. Arbitrage options abundant — multiple borrower nations, alternative financing sources, threat credibility. Experience the constraint as coordination (Rope) with asymmetric benefit.
constraint_indexing:constraint_classification(structural_adjustment_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMIST GOVERNMENT OFFICIALS (TANGLED ROPE) — Some government elites benefit from SAP conditionality (technocrats gain career advancement, certain sectors get preferential access to World Bank funding). Simultaneously, they face domestic political pressure and constrained ability to protect citizens. Experience mixed coordination (genuine fiscal discipline is needed) and extraction (conditionality removes policy autonomy). Constrained by international reputation and domestic legitimacy.
constraint_indexing:constraint_classification(structural_adjustment_doctrine, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIL SOCIETY & LABOR ORGANIZATIONS (TANGLED ROPE) — Organized resistance (unions, NGOs, protest movements) can delay or modify SAP implementation, providing some coordination function (forcing negotiation on implementation pace, exempting essential services). But suppression is high: protest is criminalized, NGO registration denied, union organizing restricted. Constrained exits: organizing under threat. Experience extraction but retain some agency through collective action.
constraint_indexing:constraint_classification(structural_adjustment_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NEOCLASSICAL CONSENSUS (PITON) — The doctrine justifies structural adjustment through economic theory (comparative advantage, efficient markets, fiscal discipline). This theoretical justification has deteriorated over 40 years of mixed empirical results: growth often lags predictions, inequality rises despite predicted convergence, and public service degradation impairs human capital accumulation. The doctrine persists through institutional inertia (IMF/World Bank credibility lock, donor coordination problems) and theater (SAP success measured by loan disbursement rate, not developmental outcomes). Theater ratio reflects gap between policy rhetoric (growth-oriented reform) and institutional reality (creditor protection mechanism).
constraint_indexing:constraint_classification(structural_adjustment_doctrine, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN — FALSE SUMMIT) — A naturalized reading claims that structural adjustment reflects immutable laws of economics: debt accumulation requires fiscal discipline, and fiscal discipline requires spending cuts and revenue enhancement. Competition requires tariff removal. These appear as natural law constraints (accessibility_collapse ≥ 0.85, resistance ≤ 0.15). However, structural data reveals this as false summit: alternative fiscal policy frameworks (Keynesian stimulus, sectoral development strategies) exist; alternative debt relief mechanisms (HIPC Initiative, odious debt doctrine) have been deployed; alternative industrial development models (infant industry protection, state-led industrialization) have succeeded in East Asia and China. The naturalization is contingent institutional choice, not law of nature.
constraint_indexing:constraint_classification(structural_adjustment_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_adjustment_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(structural_adjustment_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_adjustment_doctrine, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(structural_adjustment_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(structural_adjustment_doctrine, TR),
    TR >= 0.70.

:- end_tests(structural_adjustment_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. SAPs extract significant value from debtor nations to creditors: debt service continues at full nominal value despite economic contraction; privatized assets transfer public equity to foreign investors; currency devaluation raises effective debt burden while lowering labor costs (benefiting multinational employers); trade liberalization captures domestic market share for foreign firms. The extraction is not total (some debtor governments retain policy space, some sectors benefit from access to global markets) but is substantial and concentrated. Suppression (0.72): High. Citizens and workers lack exit options: debt obligations are sovereign-level (individuals cannot opt out); default triggers international financial isolation and capital flight; exit from labor market restricted by wage suppression and unemployment; geographic emigration available only to high-skilled minorities. Suppression increased over the measurement interval as conditionality became more detailed and intrusive. Theater ratio (0.58): Moderate-high. SAPs are marketed as 'growth-oriented structural reform' but institutional outcomes show them functioning as creditor protection mechanisms. The gap reflects that policy theory (rational expectations, efficient markets, trickle-down growth) has not matched empirical results (growth lags pre-adjustment rates in many cases, inequality rises, public service quality declines). Theater increased from 0.35 in 1980s (when SAPs had genuine growth credibility in some economies) to 0.58 by 2020s (when growth benefits are increasingly questioned but institutional mechanisms persist unchanged).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival dispersion: (rope, snare, tangled_rope, piton) across the agent spectrum. This dispersion reveals that SAP doctrine functions differently depending on position. For creditors it solves coordination (rope). For debtors it is extraction (snare). For partial insiders it is mixed (tangled_rope). For the justifying ideology it is degraded ritual (piton). The false summit (mountain) perspective tests whether SAPs are 'natural law' or 'institutional choice.' The structural data — existence of alternative fiscal frameworks (Keynesian, sectoral development, heterodox), successful alternative development models (East Asia, China), debt relief precedents (HIPC Initiative, odious debt recognition) — demonstrates contingency. SAPs are not immutable. The naturalization as 'inevitable economic law' is the rhetorical mechanism that enables extraction to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from agent power, exit options, and structural position in the extraction flow. IMF/World Bank: high institutional power + arbitrage exit (multiple debtors) + beneficiary position → d ≈ 0.05-0.15 (low d, near-zero χ or negative χ in derivative measure, experiencing the constraint as coordination with benefit). Foreign creditors: powerful institutional position + arbitrage (multiple borrowers, alternative investments) + beneficiary position → d ≈ 0.10-0.20 (very low d). Reformist technocrats: institutional power + constrained exit (international reputation and domestic legitimacy at stake) + mixed beneficiary/victim position → d ≈ 0.45-0.55 (moderate d, experiencing mixed extraction/coordination). Labor organizations: organized power + constrained exit (organizing under threat, but collective action possible) + victim position → d ≈ 0.60-0.70 (high d, experiencing high extraction but with some agency). Debtor nation citizens: powerless + trapped (no exit options from sovereign debt or labor market) + victim position → d ≈ 0.90-0.95 (maximum d, maximum experienced extraction). The engine derives these d values automatically from beneficiary/victim declarations + power + exit options, which is why the snare classification (derived from trapped/powerless perspective experiencing extraction from an institutional beneficiary) is the primary classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is resolved by showing that SAPs are not mistakenly classified as pure extraction when they serve coordination. Rather, SAPs ARE tangled rope from some institutional perspectives (reformist technocrats, development banks seeking to avoid creditor flight, some borrower governments), AND they ARE pure snare from the powerless debtor-nation citizen perspective. The mandatrophy resolution is: classify from multiple positions, recognize that the constraint has different structural properties from different locations. The snare classification from the powerless/trapped perspective is not wrong; it is the classification most relevant to understanding victim experience and policy reform. The rope classification from the creditor perspective is also not wrong; it reflects genuine coordination benefits for the creditor coalition. The piton classification reveals that the theoretical justification (neoclassical doctrine) has degraded while institutional mechanisms persist. The false summit (mountain) classification is the key diagnostic: the doctrine naturalizes contingent institutional choice as inevitable economic law. Unmasking this naturalization is the path to reform — once alternative frameworks are recognized as structurally viable (not 'economically impossible'), the constraint shifts from mountain (inevitable) to snare (unjust but changeable) or tangled_rope (genuine coordination component, but extraction component negotiable). The constraint is not at equilibrium — the measurement trajectory shows theater_ratio and extractiveness rising over 50 years, indicating institutional degradation and rent accumulation, not functional optimization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_growth_attribution,
    'How much of post-SAP growth (or stagnation) is attributable to structural adjustment versus other factors (commodity cycles, geopolitical changes, domestic policy innovation)?',
    'Synthetic control methods comparing SAP recipients to matched non-recipients; decomposition of growth drivers into policy vs external shocks; longitudinal studies isolating SAP treatment effects',
    'If SAP causally drives growth: justify snare classification requires reassessment (may be coordination-dominated). If external factors dominate: snare classification confirmed — SAP is extraction mechanism without growth payoff. If negative treatment effects: snare classification strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_growth_attribution, empirical, 'Causal attribution of post-SAP growth outcomes').

omega_variable(
    alternative_fiscal_framework_viability,
    'Do heterodox fiscal frameworks (Keynesian counter-cyclical spending, sectoral development banking, directed credit) constitute genuine alternatives with comparable debt sustainability and development outcomes?',
    'Comparative case studies of SAP vs non-SAP countries with comparable initial conditions; analysis of China, Vietnam, Botswana industrial policy trajectories; fiscal sustainability modeling under alternative frameworks',
    'If genuine alternatives exist: SAP appears as ideological choice, not technical necessity — extraction mechanism strengthens interpretation (snare rather than mountain). If alternatives prove unstable: SAP gains technical justification, shifting toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_fiscal_framework_viability, empirical, 'Viability of alternative fiscal and industrial policy frameworks').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.72) primarily structural (debt obligations, capital flight risk, IMF compliance requirements) or internalized (technocrats have adopted neoclassical worldview, opposition elites lack confidence in alternatives)?',
    'Discourse analysis of government policy documents; interviews with policymakers; examination of post-IMF program outcomes under alternative leadership; tracking of policy reversals when external pressure relaxes',
    'If structural: suppression persists even when external pressure removed — high-cost exit remains default. If internalized: suppression declines when alternative ideologies gain credibility — exit becomes conceivable. Mix of both suggests hybrid mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in policymaking elite').

omega_variable(
    creditor_coordination_necessity,
    'Do creditors genuinely need IMF coordination mechanism for debt enforcement, or does SAP persist primarily to justify institutional expansion and staffing?',
    'Historical comparison of pre-IMF debt crises to post-IMF debt management; analysis of debt recovery rates with vs without SAP conditions; examination of bilateral creditor negotiations',
    'If coordination is necessary: rope component of tangled_rope becomes prominent — genuine coordination problem exists alongside extraction. If institutional self-interest dominates: snare classification strengthens — mechanism is extraction theater with coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_coordination_necessity, empirical, 'Whether SAP is necessary for creditor coordination versus institutional self-perpetuation').

omega_variable(
    exit_option_reality_for_debtor_states,
    'What are realistic exit options for a debtor nation under SAP? Do defaults, alternative lenders (China), or heterodox policy constitute genuine exits, or do they trigger costs that re-trap the nation?',
    'Case study analysis of default outcomes (Argentina, Ecuador); tracking of nations that exited IMF programs early; cost-benefit analysis of alternative financing (comparing Chinese loans, Brady bonds, debt relief programs)',
    'If exits are illusory (default creates worse outcomes): ''trapped'' classification confirmed, snare classification strengthened. If exits are viable with acceptable costs: exit_options upgrade to ''constrained'' rather than ''trapped'' — this would downshift snare toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_reality_for_debtor_states, empirical, 'Viability and cost of exit options for SAP-bound debtor nations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(structural_adjustment_doctrine, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stru_tr_t0, structural_adjustment_doctrine, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stru_tr_t15, structural_adjustment_doctrine, theater_ratio, 15, 0.48).
narrative_ontology:measurement(stru_tr_t30, structural_adjustment_doctrine, theater_ratio, 30, 0.58).
narrative_ontology:measurement(stru_tr_t45, structural_adjustment_doctrine, theater_ratio, 45, 0.58).

% Extraction over time
narrative_ontology:measurement(stru_be_t0, structural_adjustment_doctrine, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(stru_be_t15, structural_adjustment_doctrine, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(stru_be_t30, structural_adjustment_doctrine, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(stru_be_t45, structural_adjustment_doctrine, base_extractiveness, 45, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(structural_adjustment_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(structural_adjustment_doctrine, debt_trap_lending).
narrative_ontology:affects_constraint(structural_adjustment_doctrine, labor_market_deregulation).
narrative_ontology:affects_constraint(structural_adjustment_doctrine, commodity_export_dependency).
narrative_ontology:affects_constraint(structural_adjustment_doctrine, public_service_privatization).

% DUAL FORMULATION NOTE:
% Structural adjustment doctrine is the superordinate institutional mechanism that coordinates multiple subordinate extraction constraints. Each subordinate constraint (debt traps, labor deregulation, public service privatization) has its own ε value and operates through specific sectoral mechanisms. SAP doctrine operates at the level of conditionality architecture and justification. Decomposition: SAP doctrine itself is ε=0.68, snare-dominant; specific sectoral implementations (e.g., healthcare privatization) may have ε=0.75+, snare-dominant but with higher specificity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(structural_adjustment_doctrine, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
