% ============================================================================
% CONSTRAINT STORY: risk_socialization_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_risk_socialization_threshold, []).

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
 *   constraint_id: risk_socialization_threshold
 *   human_readable: The Asymmetric Liability Trap
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Asymmetric Liability Trap emerges from the structural combination of
 *   three elements: (1) concentration of financial system assets in a small
 *   number of entities, (2) implicit guarantees that these entities will be
 *   rescued in a crisis to prevent systemic collapse, and (3) asymmetric
 *   profit distribution where gains during stability accrue to equity holders
 *   and management while losses during crisis are socialized across the
 *   taxpayer base. This constraint exhibits a perspectival range from pure
 *   snare (powerless taxpayers and smaller firms) through tangled rope
 *   (regulators with dual mandates) to rope (the systemically critical firm
 *   itself). The asymmetry is not accidental — it is structurally enforced
 *   through political economy, regulatory capture, and the logic of systemic
 *   risk. TBTF entities capture supernormal rents during stable periods
 *   through higher leverage, lower capital requirements, and reduced
 *   regulatory overhead compared to non-systemic competitors. During crises,
 *   losses that would ordinarily bankrupt the entity are transferred to the
 *   general taxpayer base through central bank liquidity provision,
 *   government guarantees, and future deficit spending. The constraint is
 *   self-reinforcing: as TBTF entities grow larger and more interconnected,
 *   the implicit guarantee becomes more valuable and more politically
 *   necessary, increasing the incentive to accept extraction as the price of
 *   financial stability.
 *
 * KEY AGENTS:
 *   - Systemically Critical Firm (e.g., JPMorgan Chase, Deutsche Bank): Primary beneficiary (institutional/arbitrage) — captures supernormal rents from lower cost of capital and regulatory forbearance during stability; protected from bankruptcy during crisis
 *   - General Taxpayer Base: Primary victim (powerless/trapped) — bears full cost of loss socialization through deficit spending and inflation; no exit options within the national tax system
 *   - Smaller Competitive Firms: Secondary victim (powerless/trapped) — face competitive disadvantage from implicit subsidy to TBTF entities; during crisis, lose market share and customer deposits to rescued incumbents
 *   - Central Bank: Institutional actor with constrained exit (institutional/constrained) — mandated to maintain financial stability and prevent systemic collapse; must implement the implicit guarantee even if recognizing its extractive properties
 *   - Competitive Regulators (SEC, OCC, Federal Reserve): Organized actors with dual mandates (organized/constrained) — tasked with both systemic stability and market competition; experience tangled rope classification from need to maintain both simultaneously
 *   - International Financial Standards Bodies (Basel Committee, FSB): Organized agents attempting sunset through capital regulation (organized/constrained) — building scaffolding to reduce systemic leverage over time
 *   - Analytical Observer: Sees the coordination-extraction hybrid (analytical/analytical) — observes that TBTF simultaneously enables large-scale intermediation AND privatizes gains while socializing losses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(risk_socialization_threshold, 0.58).
domain_priors:suppression_score(risk_socialization_threshold, 0.68).
domain_priors:theater_ratio(risk_socialization_threshold, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(risk_socialization_threshold, extractiveness, 0.58).
narrative_ontology:constraint_metric(risk_socialization_threshold, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(risk_socialization_threshold, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(risk_socialization_threshold, tangled_rope).
narrative_ontology:human_readable(risk_socialization_threshold, "The Asymmetric Liability Trap").
narrative_ontology:topic_domain(risk_socialization_threshold, "economic/political").

domain_priors:requires_active_enforcement(risk_socialization_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(risk_socialization_threshold, systemically_critical_firm).
narrative_ontology:constraint_beneficiary(risk_socialization_threshold, executive_class).
narrative_ontology:constraint_victim(risk_socialization_threshold, general_taxpayer_base).
narrative_ontology:constraint_victim(risk_socialization_threshold, competitive_firm_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERAL TAXPAYER (SNARE) — Trapped within the tax base. Cannot exit the obligation to fund systemic entity bailouts. Bears full cost of loss socialization without corresponding profit during stability. Extraction mechanism enforced through tax law and deficit spending. Zero alternatives or substitutes.
constraint_indexing:constraint_classification(risk_socialization_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALLER COMPETITIVE FIRMS (SNARE) — Trapped in ecosystem where systemic entity receives implicit subsidy (bailout guarantee) that smaller competitors cannot access. Suppression is extreme: during crisis, capital flows to rescued incumbent; normal market discipline is suspended; exit option is acquisition or bankruptcy, not survival with dignity. Extraction manifests as lost market share and competitive disadvantage.
constraint_indexing:constraint_classification(risk_socialization_threshold, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEMICALLY CRITICAL FIRM (ROPE) — Experiences the constraint as coordination mechanism: the implicit guarantee enables risk-taking and leverage that funds growth and market liquidity. Profits during stability; losses are mutualized during crisis. Effective exit options exist (can relocate, merge, restructure) and are used to extract maximum value. Net beneficiary position.
constraint_indexing:constraint_classification(risk_socialization_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COMPETITIVE MARKET REGULATORS (TANGLED ROPE) — Possess dual mandates: maintain systemic stability (beneficiary incentive to support TBTF framework) and preserve market competition (victim incentive to eliminate implicit subsidies). Constrained by political economy — cannot eliminate the framework without risking financial collapse; cannot sustain it without gutting competitive markets. Experiences both coordination (stability funding) and extraction (moral hazard, competitive distortion).
constraint_indexing:constraint_classification(risk_socialization_threshold, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL FINANCIAL STANDARDS BODIES (SCAFFOLD) — Basel III, dodd-frank, leverage ratio requirements, and stress testing regimes represent attempts to build sunset logic into TBTF. Designed as temporary scaffolding: as capital buffers accumulate and systemic risk concentration declines, the implicit guarantee should become unnecessary. Theater_ratio is moderate (regulatory compliance theater is high but the underlying capital requirements are functional). Sunset clause is implicit: when systemic leverage falls below threshold, explicit bailout guarantee is no longer needed.
constraint_indexing:constraint_classification(risk_socialization_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL REGULATORY FRAMEWORK (PITON) — The original post-2008 regulatory framework (Glass-Steagall separation, circuit breaker mechanisms, deposit insurance caps) has atrophied. Its structural function (separating risky trading from deposit-backed operations) is largely ceremonial; lobbying and legislative capture have hollowed its enforcement. Maintained through institutional inertia and the political cost of explicit repeal. Theater ratio is high because the ritual of regulatory compliance persists even as the functional constraint erodes.
constraint_indexing:constraint_classification(risk_socialization_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical context, the asymmetric liability trap exhibits both coordination (enables large-scale financial intermediation and leverage that supports productive investment) and extraction (privatized gains, socialized losses). The extraction is measurable: difference between expected cost of implicit guarantee and premium paid by TBTF entities. The coordination is real: without leverage and risk-taking, some productive investment does not occur. The tension is structural, not a misallocation that can be solved by tweaking parameters.
constraint_indexing:constraint_classification(risk_socialization_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(risk_socialization_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(risk_socialization_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(risk_socialization_threshold, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(risk_socialization_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(risk_socialization_threshold, TR),
    TR >= 0.70.

:- end_tests(risk_socialization_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The asymmetric liability trap extracts rents from taxpayers and competitive firms through two mechanisms: (1) direct transfers during crises (explicit bailouts totaling trillions of dollars across 2008-2009 global crisis), and (2) continuous extraction during stability through lower cost of capital, regulatory forbearance, and implicit subsidy that accrues as supernormal profits to TBTF shareholders and executives. The value 0.58 reflects the observation that some portion of the TBTF structure is genuinely coordination (enabling leverage and intermediation that supports real investment) — if the entirety were pure extraction, ε would be >0.70. But the preponderance of the mechanism is asymmetric profit-taking. Suppression (0.68): High. Suppression manifests through multiple channels: (1) political economy — TBTF entities lobby heavily to prevent decomposition and maintain the implicit guarantee; (2) regulatory capture — agency officials rotate between industry and regulatory positions, embedding TBTF logic into enforcement; (3) conceptual capture — the systemic stability rationale for TBTF becomes naturalized as inherent to finance rather than contingent institutional choice; (4) technological entanglement — smaller competitors cannot match the scale and leverage that TBTF entities deploy, making exit from the competitive disadvantage structurally impossible. Theater ratio (0.64): Moderate-high. Post-2008 regulatory responses (Dodd-Frank, Basel III, stress testing) create significant theater: compliance procedures are elaborate and performative, but functional constraints remain negotiable through lobbying and regulatory accommodation. The theater has increased over the interval as regulatory complexity has accumulated without corresponding reduction in TBTF status.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap appears between the TBTF entity (Rope: sees coordination, net beneficiary position) and the general taxpayer (Snare: sees pure extraction, full target position). These are the same constraint with opposite experiential content. The tangled rope perspective from regulators occupies the middle ground — they experience both the coordination function (maintaining financial stability, enabling credit markets) and the extraction function (subsidizing TBTF, distorting competition). The scaffold perspective from international bodies represents an attempt to collapse the perspectival gap over time by reducing systemic leverage so the implicit guarantee becomes unnecessary — a long-term transition from snare/tangled rope to rope/no constraint. The piton perspective observes that the historical regulatory constraint has degraded (regulatory capture has hollowed Glass-Steagall) without the formal rule being repealed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's position in the profit-loss transfer chain. TBTF entities have d ≈ 0.05-0.15 (full beneficiaries with arbitrage exit) — they capture profits during stability and are rescued during crises, experiencing effective negative extraction (the constraint subsidizes them). Taxpayers have d ≈ 0.90-1.00 (full targets with trapped exit) — they pay during crises and receive no corresponding benefit during stability, experiencing maximum extraction. Smaller competitors have d ≈ 0.85-0.95 (victims with trapped exit in the competitive market) — they cannot access the implicit guarantee available to TBTF, creating a persistent competitive disadvantage enforced by market structure. Regulators have d ≈ 0.55-0.70 (mixed position, constrained exit) — they are neither pure targets nor pure beneficiaries; they must maintain the system even while recognizing its extractive properties. The analytical observer has d ≈ 0.72 (neutral position, analytical exit), able to measure the extraction magnitude without bearing its cost directly.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED. The asymmetric liability trap sits at extractiveness 0.58, below the 0.70 threshold that requires mandatrophy resolution, but at sufficient height that the coordination-extraction mix is structurally ambiguous. The mandatrophy question is: 'Is TBTF a necessary coordination mechanism for modern finance, or is it a system of extraction that naturalizes itself as coordination?' The unresolved status reflects genuine deep disagreement in the financial economics community. One position: TBTF is coordination — the implicit guarantee enables leverage and liquidity that fund productive investment; without it, credit markets would be less efficient and growth would decline. Alternative position: TBTF is extraction — the productivity gains are exceeded by moral hazard, competitive distortion, and the transfer of wealth to executives and shareholders; the system could be stabilized through harder constraints (mandatory breakup, higher capital requirements, explicit resolution mechanisms) without loss of net productivity. The field exhibits no consensus on which position is correct. Resolution mechanism would require: (1) empirical measurement of the annual implicit subsidy value, (2) comparison to productivity gains from enabled intermediation, (3) counterfactual analysis of how financial systems without TBTF status would function, and (4) political determination of acceptable trade-offs between stability and distribution. Until these are resolved, the constraint remains tangled rope rather than moving to explicit snare or rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_subsidy_quantification,
    'What is the precise annual value of the implicit TBTF subsidy measured as the difference between the entity''s actual cost of capital and the cost it would pay in a competitive market without bailout guarantee?',
    'Credit spread analysis comparing TBTF institutions to non-systemically-critical peers; comparison of borrowing costs before vs after crisis bailout announcements; pricing of credit default swaps relative to equity valuations; regulatory stress test scenarios with explicit removal of bailout guarantee',
    'If subsidy > 50 basis points annually across major TBTF entities: extractiveness classification increases to 0.65+, moving firmly into Snare territory. If subsidy < 20 basis points: tangled_rope classification is more defensible; extraction component is smaller.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implicit_subsidy_quantification, empirical, 'Quantification of the annual implicit bailout subsidy').

omega_variable(
    systemic_leverage_threshold,
    'At what level of aggregate leverage in the financial system does the TBTF constraint transition from enabling productive intermediation (coordination function) to creating fragility (extraction function)?',
    'Historical analysis of leverage cycles and crisis triggers; comparison of leverage ratios before 2008 crisis vs post-Basel-III periods; macroeconomic modeling of optimal leverage levels for financial stability vs deadweight loss from regulatory constraints',
    'If threshold is currently exceeded: coordination rationale for TBTF is invalid, classification shifts to pure Snare. If threshold has safety margin: scaffold sunset logic is realistic, TBTF can degrade to lower extractiveness over time as leverage falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_leverage_threshold, empirical, 'Leverage threshold distinguishing coordination from extraction in TBTF').

omega_variable(
    moral_hazard_behavioral_coupling,
    'To what extent do TBTF entities increase risk-taking in response to perceived bailout guarantee, and is this increase offset by productivity gains from enabled intermediation?',
    'Comparison of risk metrics (leverage, loan-to-value ratios, derivatives exposure, off-balance-sheet vehicles) for TBTF vs non-TBTF institutions controlling for size and market segment; correlation between regulatory tightening events and actual risk behavior; measurement of net NPV contribution from TBTF intermediation vs expected cost of moral hazard losses',
    'If moral hazard exceeds productivity gains: extraction dominates, classification confirms Snare. If productivity gains exceed hazard costs: coordination function is real, tangled_rope classification holds, extraction component is justified as price of systemic stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_hazard_behavioral_coupling, empirical, 'Whether moral hazard from TBTF guarantee exceeds coordination benefits').

omega_variable(
    political_reform_feasibility,
    'Is it politically feasible to implement a hard constraint (e.g., mandatory breakup or credible pre-crisis resolution authority) that eliminates TBTF status within a 10-20 year horizon, or is the constraint self-perpetuating through captured regulation?',
    'Analysis of lobbying expenditures, regulatory capture metrics, legislative voting patterns; historical comparison to other decomposed monopolies or national banking systems that eliminated TBTF status; public opinion tracking on financial system reform proposals',
    'If reform is feasible: scaffold sunset is real, TBTF is a temporary constraint that can transition to lower extractiveness. If reform is not feasible: TBTF is self-perpetuating (piton-like), extraction will persist unless external shock forces change, classification should emphasize permanence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_reform_feasibility, preference, 'Political feasibility of eliminating TBTF through hard constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(risk_socialization_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(risk_soc_tr_t0, risk_socialization_threshold, theater_ratio, 0, 0.55).
narrative_ontology:measurement(risk_soc_tr_t5, risk_socialization_threshold, theater_ratio, 5, 0.62).
narrative_ontology:measurement(risk_soc_tr_t10, risk_socialization_threshold, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(risk_soc_be_t0, risk_socialization_threshold, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(risk_soc_be_t5, risk_socialization_threshold, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(risk_soc_be_t10, risk_socialization_threshold, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(risk_socialization_threshold, resource_allocation).
narrative_ontology:affects_constraint(risk_socialization_threshold, regulatory_capture_dynamic).
narrative_ontology:affects_constraint(risk_socialization_threshold, systemic_risk_concentration).
narrative_ontology:affects_constraint(risk_socialization_threshold, moral_hazard_amplification).

% DUAL FORMULATION NOTE:
% The asymmetric liability trap decomposes into three downstream constraints: (1) regulatory_capture_dynamic — the mechanism by which TBTF entities prevent reform through political economy; (2) systemic_risk_concentration — the structural accumulation of leverage and interconnectedness that makes TBTF entities critical to system stability; (3) moral_hazard_amplification — the behavioral response of TBTF entities to the implicit guarantee, which increases riskiness over time. Each has its own extractiveness value. The trap itself (this story) represents the integrative constraint that couples all three together.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(risk_socialization_threshold, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
