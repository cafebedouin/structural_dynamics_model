% ============================================================================
% CONSTRAINT STORY: asian_infrastructure_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asian_infrastructure_debt_trap, []).

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
 *   constraint_id: asian_infrastructure_debt_trap
 *   human_readable: Asian Infrastructure Debt Trap
 *   domain: economic_geopolitics
 *
 * SUMMARY:
 *   The Asian infrastructure debt trap is a structural constraint in which
 *   external creditors (primarily China, Japan, multilateral development
 *   banks) finance large-scale infrastructure projects in developing Asian
 *   nations through loans structured with conditions that transfer risk
 *   entirely to borrowers. The mechanism operates as follows: creditors
 *   finance projects with optimistic return projections; domestic elites
 *   capture portions through inflated contracts and corruption; projects
 *   underperform expectations; debt servicing becomes unaffordable; borrower
 *   nations face austerity, sovereign renegotiation from a position of
 *   weakness, or implicit vassalage through debt-to-equity swaps and
 *   strategic asset seizure. The constraint exhibits maximum extractiveness
 *   and suppression characteristic of snares: the borrowing nation is
 *   structurally trapped (cannot exit without collapse), alternatives are
 *   suppressed (renegotiation foreclosed by conditionality), and the
 *   extraction is systematic (creditor benefits regardless of project
 *   success). Theater ratio reflects that development rhetoric
 *   (infrastructure as shared prosperity, capacity building) masks the
 *   extraction mechanism. The constraint's extractiveness has increased over
 *   the interval as debt burdens have accumulated and creditor leverage has
 *   grown.
 *
 * KEY AGENTS:
 *   - Creditor Nations/Institutions: Primary beneficiaries (institutional/arbitrage) — gain interest flows, geopolitical leverage, strategic asset access through renegotiation; exit easily by moving to new borrowers
 *   - Debtor Nation Population: Primary victims (powerless/trapped) — bear full cost through reduced public services, austerity, intergenerational obligation; no exit capacity
 *   - Debtor Nation Government: Secondary victim (moderate/constrained) — faces structural mobility (could default) but suppressed by both creditor leverage and domestic capture; constrained exit
 *   - Domestic Elite: Beneficiary-victims (powerful/mobile) — capture project contracts and corruption rents; insulated from repayment burden through asset externalization; paradoxically trapped by maintaining extraction system
 *   - Infrastructure Contractors: Incidental beneficiaries (institutional/arbitrage) — gain contracts and cost-plus arrangements; exit easily with no ongoing obligation
 *   - Analytical Observer: Views constraint as designed extraction mechanism disguised as development assistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asian_infrastructure_debt_trap, 0.68).
domain_priors:suppression_score(asian_infrastructure_debt_trap, 0.65).
domain_priors:theater_ratio(asian_infrastructure_debt_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asian_infrastructure_debt_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(asian_infrastructure_debt_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(asian_infrastructure_debt_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asian_infrastructure_debt_trap, snare).
narrative_ontology:human_readable(asian_infrastructure_debt_trap, "Asian Infrastructure Debt Trap").
narrative_ontology:topic_domain(asian_infrastructure_debt_trap, "economic_geopolitics").

domain_priors:requires_active_enforcement(asian_infrastructure_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asian_infrastructure_debt_trap, creditor_nations).
narrative_ontology:constraint_beneficiary(asian_infrastructure_debt_trap, infrastructure_contractors).
narrative_ontology:constraint_beneficiary(asian_infrastructure_debt_trap, debt_servicing_intermediaries).
narrative_ontology:constraint_victim(asian_infrastructure_debt_trap, debtor_nations).
narrative_ontology:constraint_victim(asian_infrastructure_debt_trap, local_populations).
narrative_ontology:constraint_victim(asian_infrastructure_debt_trap, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBTOR NATION POPULATION (SNARE) — Trapped by structural debt incurred for infrastructure projects that benefit elites or external actors. No exit capacity: sovereign debt cannot be escaped without economic collapse. Bears full extraction burden through austerity, reduced public services, and intergenerational obligation. Maximum suppression: alternatives (domestic financing, reduced project scope, renegotiation) are foreclosed by creditor conditions and domestic political capture.
constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: DEBTOR NATION GOVERNMENT (SNARE) — Constrained but facing severe costs to exit: default triggers capital flight, currency collapse, and loss of future borrowing capacity. Structurally mobile (could default) but politically trapped by domestic elites who benefit from the debt arrangement. Exit options exist but extraction remains severe because the barriers are reinforced by both creditor leverage and domestic capture.
constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR NATION (ROPE) — Experiences the constraint as coordination: infrastructure financing solves a genuine collective action problem (developing nations need capital, creditors have surplus). Benefits through interest repayment, currency flows, and geopolitical leverage. Exits easily (walks away from non-performing loans, diversifies portfolio) or switches to new borrowers. Net beneficiary with minimal experienced extraction.
constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC ELITE (TANGLED ROPE) — Benefits directly from infrastructure contracts, land seizures, and development projects. Also trapped by the debt structure they created: their wealth depends on maintaining the extraction system. Some exit capacity (capital flight, asset diversification) but also benefits from the constraint. Coordinating function: they negotiated the financing that enabled the infrastructure. Extraction: they have insulated themselves from repayment burden through corruption and asset externalization, leaving the public to service the debt.
constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INFRASTRUCTURE CONTRACTOR (ROPE) — Pure coordination: the debt financing solves the problem of project capital. Contractor benefits through contracts and cost-plus arrangements. Exits easily (completes project, moves to next market) with no ongoing obligation. Minimal extraction from their perspective — the debt structure enables their business and imposes no cost on them.
constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a global/civilizational view, the infrastructure debt trap is a deliberate extraction mechanism disguised as development assistance. The pattern repeats: loans for infrastructure projects that over-estimate usage/returns, contracts inflated through corruption, debt servicing that crowds out healthcare and education, and eventual sovereign renegotiation from a position of weakness. The constraint's design (high suppression, low exit capacity, asymmetric information) reveals the extraction mechanism. Theater ratio reflects that development rhetoric masks extractive intent.
constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asian_infrastructure_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asian_infrastructure_debt_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asian_infrastructure_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asian_infrastructure_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. Creditors capture interest flows, currency benefits, and geopolitical leverage. Borrower nations service debt through austerity, reduced public investment, and fiscal constraint. The trajectory from 0.35 to 0.68 reflects debt accumulation effects: early loans appear manageable; cumulative burden becomes unaffordable; creditors use debt leverage to extract non-financial concessions (strategic ports, mining rights, voting alignment). Suppression (0.65): High. Borrowers face multiple barriers to exit: (1) domestic political capture — elites benefit from debt-financed projects and oppose restructuring; (2) capital flight risk — sovereign restructuring triggers immediate capital outflows; (3) loss of future borrowing capacity — default closes development finance access; (4) conditionality — structural adjustment programs mandate austerity and privatization regardless of local need. Theater ratio (0.58 and increasing): Moderate-high. Development rhetoric (infrastructure as shared growth, capacity building) frames what is structurally extraction. Project evaluations consistently overestimate returns; creditors present optimistic scenarios as analysis rather than marketing; borrower governments display commitment through austerity and reform theater rather than through improved project returns. Theater increase reflects that as extraction mechanisms mature, more performative work is required to maintain legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six distinct experiential realities from the same structural base: (1) Creditors experience pure coordination and zero extraction. (2) Contractors experience pure coordination, project enablement, and zero extraction. (3) Domestic elite experience mixed benefit and trap — they benefit from rents but are imprisoned in the system they created. (4) Debtor government experiences high extraction, constrained alternatives, and internal pressure from elites to maintain the system. (5) Debtor population experiences maximum extraction, no alternatives, and intergenerational burden. (6) Analytical observer sees the mechanism as designed extraction masked by development rhetoric. The gap between perspectives 1-2 (creditors/contractors) and perspectives 4-5 (debtors) is maximal: the same infrastructure financing is experienced as beneficial coordination and as extractive trapping. This gap is not perceptual — it reflects the structural asymmetry of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Creditors: Beneficiary status + arbitrage exit → low d (0.05-0.15) → negative f(d) → negative experienced extractiveness. They are the pure beneficiary with full exit capacity. Contractors: Beneficiary status + arbitrage exit → low d → negative f(d). Domestic elite: Mixed beneficiary-victim status + mobile exit → mid d (0.35-0.45) → positive f(d) but lower than trapped agents. They benefit from project rents but are trapped by the system they maintain. Debtor government: Victim status + constrained exit → high d (0.70-0.80) → high f(d) (1.10+). Structurally mobile but suppressed by both external and domestic leverage; constrained exit is expensive. Debtor population: Victim status + trapped exit → maximum d (0.90-0.98) → maximum f(d) (1.35+). Cannot exit; bears full extraction. Analytical observer: Neutral structural position + analytical exit → canonical d (0.73) → f(d) ≈ 1.15. Sees full extractive mechanism from outside. The derived d values show that victims experience 8-10x the effective extractiveness that beneficiaries experience, confirming the snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint's mandatrophy is resolved by distinguishing elite capture (domestic) from structural extraction (international). The constraint is NOT a coordination mechanism that happens to be unfair — it is a snare designed to extract through debt leverage. Resolution chain: (1) Infrastructure projects are real coordination (different classification if evaluated independently). (2) Debt financing adds extraction layer: loans structured with optimistic projections and high conditionality. (3) Domestic capture amplifies extraction: elites gain contracts and asset externalization; public bears repayment. (4) Creditor leverage compounds extraction: debt servicing enforces austerity and policy compliance. (5) Analytical observation reveals design: the combination of optimistic projections, constrained renegotiation, and creditor leverage is not accident but architecture. The constraint is a snare because: ε ≥ 0.46 (confirmed at 0.68), suppression ≥ 0.60 (confirmed at 0.65), χ ≥ 0.66 (derived as 0.68 × f(d=0.95) × σ(regional=0.9) ≈ 0.99 for trapped debtor population). All three snare gates pass. False Rope diagnosis prevented because: (1) beneficiaries listed (creditor nations, contractors) and victims listed (debtor nations, populations). (2) Perspectives show zero beneficiary-to-victim transmission of benefits — creditors gain from interest, not from project success. (3) Theater ratio increase (0.42→0.58) indicates development rhetoric masking extraction, not genuine growth from infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_return_calculation,
    'Are the expected economic returns from financed infrastructure projects genuinely underestimated, or are they systematically misrepresented to justify larger loans?',
    'Retrospective analysis: compare projected vs actual traffic volumes, revenue streams, and economic multipliers for completed projects across 20+ major infrastructure loans',
    'If underestimated: constraint is partly coordination failure (Rope from creditor view). If systematically misrepresented: constraint is designed extraction (Snare confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_return_calculation, empirical, 'Whether infrastructure return projections are genuine errors or deliberate misrepresentation').

omega_variable(
    domestic_elite_capture_depth,
    'How much of the debt load results from elite capture (inflated contracts, corruption, asset externalization) vs legitimate infrastructure underperformance?',
    'Forensic analysis of contract pricing vs regional comparables; tracking of asset flows and capital flight during and after loan periods; correlation between governance transparency and debt sustainability',
    'If capture dominates: tangled rope classification for domestic elite stands (they created the trap). If underperformance dominates: constraint shifts toward pure coordination failure (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_elite_capture_depth, empirical, 'Proportion of debt attributable to elite capture vs infrastructure underperformance').

omega_variable(
    creditor_knowledge_and_intent,
    'Do creditors systematically know that their project evaluations are optimistic, and do they lend anyway because the debt structure transfers risk to borrowers regardless of project success?',
    'Documentary analysis of loan origination documents, risk assessments, and internal creditor communications; comparison of same creditor''s evaluation standards across similar projects in different geographies',
    'If creditors knew: snare classification confirmed as deliberate design. If creditors did not know: constraint is coordination failure with asymmetric information (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_knowledge_and_intent, empirical, 'Whether creditors knowingly lend despite weak project fundamentals').

omega_variable(
    exit_option_reality,
    'Can debtor nations realistically restructure or default without catastrophic costs, or is the suppression of alternatives total?',
    'Historical case studies of debt restructuring outcomes; comparison of post-restructuring growth vs post-austerity growth; tracking of capital market re-access timelines after defaults',
    'If restructuring is feasible: exit_options should be ''constrained'' not ''trapped'' for some perspectives. If suppression is total: mountain-level immutability applies to some agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_reality, empirical, 'Whether default/restructuring is genuinely infeasible or merely high-cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asian_infrastructure_debt_trap, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asiadebt_tr_t0, asian_infrastructure_debt_trap, theater_ratio, 0, 0.42).
narrative_ontology:measurement(asiadebt_tr_t5, asian_infrastructure_debt_trap, theater_ratio, 5, 0.5).
narrative_ontology:measurement(asiadebt_tr_t10, asian_infrastructure_debt_trap, theater_ratio, 10, 0.58).
narrative_ontology:measurement(asiadebt_tr_t15, asian_infrastructure_debt_trap, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(asiadebt_be_t0, asian_infrastructure_debt_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(asiadebt_be_t5, asian_infrastructure_debt_trap, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(asiadebt_be_t10, asian_infrastructure_debt_trap, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(asiadebt_be_t15, asian_infrastructure_debt_trap, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asian_infrastructure_debt_trap, resource_allocation).
narrative_ontology:affects_constraint(asian_infrastructure_debt_trap, sovereign_debt_sustainability).
narrative_ontology:affects_constraint(asian_infrastructure_debt_trap, resource_extraction_concessions).
narrative_ontology:affects_constraint(asian_infrastructure_debt_trap, port_militarization).
narrative_ontology:affects_constraint(asian_infrastructure_debt_trap, monetary_policy_capture).

% DUAL FORMULATION NOTE:
% The infrastructure debt trap is downstream of specific creditor-debtor relationships but represents a distinct structural constraint in the architecture of global development finance. Upstream constraints (creditor lending practices, debtor governance capacity) affect the parameters; downstream constraints (port control, mining concessions, monetary policy dependency) are enabled by debt leverage. Family decomposition: (1) infrastructure_debt_trap — the primary extraction mechanism via loan structuring; (2) sovereign_debt_sustainability — the ecological consequence of debt accumulation; (3) geopolitical_leverage_through_debt — the downstream institutional control mechanism. Each has different ε values reflecting different observable (direct cash flows, fiscal sustainability, strategic control).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asian_infrastructure_debt_trap, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
