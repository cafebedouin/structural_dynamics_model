% ============================================================================
% CONSTRAINT STORY: corporate_governance_voting_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_corporate_governance_voting_control, []).

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
 *   constraint_id: corporate_governance_voting_control
 *   human_readable: Corporate Governance Voting Control Mechanisms
 *   domain: corporate/governance/capital_markets
 *
 * SUMMARY:
 *   Corporate governance voting control creates a structural tension between
 *   the principle of shareholder democracy (one share = one vote) and the
 *   practice of capital concentration (controlling shareholders hold
 *   disproportionate power). This constraint exhibits asymmetric extraction
 *   layered over a genuine coordination function: firms require
 *   decision-making authority and strategic direction, which voting control
 *   nominally provides. However, voting mechanisms are used to extract
 *   economic benefit in excess of the controller's proportional ownership
 *   stake through dilutive share issuances, related-party transactions,
 *   excess compensation, and blocked board access for minorities. The
 *   constraint operates in a global capital market context where dual-class
 *   shares, voting agreements, and charter-based control mechanisms are
 *   explicitly designed to maintain controller power against diffuse
 *   shareholder interests. The theater ratio (0.68) reflects that formal
 *   governance mechanisms (proxy contests, say-on-pay votes, board
 *   independence requirements, disclosure rules) perform accountability
 *   without substantively constraining controlling shareholder actions in
 *   most cases. Minority shareholders can theoretically initiate proxy
 *   contests or shareholder proposals, but these mechanisms have
 *   extraordinarily low success rates (~3-8% for director elections in
 *   contested situations) and high activation barriers. Institutional
 *   investors benefit from coordination mechanisms that enable collective
 *   voice (shareholder coalitions, proxy advisory recommendations) but remain
 *   constrained by fiduciary duty limitations and information asymmetries.
 *   Open Science Coalition equivalent: SEC regulatory reforms, institutional
 *   investor coordination networks, and shareholder activism infrastructure
 *   are building alternative pathways (proxy access, majority voting,
 *   beneficial ownership disclosure) with explicit sunset logic as
 *   shareholder governance norms mature.
 *
 * KEY AGENTS:
 *   - Minority Shareholders: Primary victim (powerless/trapped) — sunk capital in illiquid positions, mathematically irrelevant voting power in concentrated firms, subject to dilution and related-party extraction
 *   - Controlling Shareholders (founders, families, strategic investors): Primary beneficiary (institutional/arbitrage) — capture voting control without proportional economic ownership, convert voting power into governance rights and economic benefit
 *   - Institutional Investors (mutual funds, pension funds, asset managers): Secondary victim/moderate actor (moderate/constrained) — benefit from voting coordination mechanisms but face high activation costs and fiduciary constraints on exit
 *   - Board of Directors: Formal governance institution (institutional/arbitrage) — exercise nominal fiduciary duty; functionally implement controller decisions; maintain theater through governance performance
 *   - SEC and Regulatory Bodies: Scaffolding institution (organized/constrained) — implement temporary coordination mechanisms (proxy access, say-on-pay, disclosure) with transitional logic as governance norms mature
 *   - Proxy Advisory Firms (ISS, Glass Lewis): Institutional mediators (institutional/arbitrage) — amplify minority voice but face their own capture risks and institutional interests
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent voting structures (dual-class shares, voting agreements) as immutable features of corporate property rights
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(corporate_governance_voting_control, 0.58).
domain_priors:suppression_score(corporate_governance_voting_control, 0.65).
domain_priors:theater_ratio(corporate_governance_voting_control, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(corporate_governance_voting_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(corporate_governance_voting_control, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(corporate_governance_voting_control, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(corporate_governance_voting_control, tangled_rope).
narrative_ontology:human_readable(corporate_governance_voting_control, "Corporate Governance Voting Control Mechanisms").
narrative_ontology:topic_domain(corporate_governance_voting_control, "corporate/governance/capital_markets").

domain_priors:requires_active_enforcement(corporate_governance_voting_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(corporate_governance_voting_control, controlling_shareholders).
narrative_ontology:constraint_beneficiary(corporate_governance_voting_control, management_aligned_investors).
narrative_ontology:constraint_victim(corporate_governance_voting_control, minority_shareholders).
narrative_ontology:constraint_victim(corporate_governance_voting_control, stakeholder_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MINORITY SHAREHOLDER (SNARE) — Trapped by sunk capital and illiquidity costs. Holds voting rights that are mathematically irrelevant in firms with concentrated control (>50% held by founder/family/institution). Cannot exit without realizing loss or waiting for acquisition. Experiences full extractive force: dilutive issuances, related-party transactions, excess management compensation bypassing their vote.
constraint_indexing:constraint_classification(corporate_governance_voting_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL INVESTOR (TANGLED ROPE) — Constrained by fiduciary duty to hold diversified portfolios; exit cost is not zero (rebalancing friction, market impact, opportunity cost) but not insurmountable. Benefits from voting coordination mechanisms (shareholder proposals, proxy contests) that enable collective voice but face high activation barriers. Moderate extraction: blocked from board seats, excluded from information access, but retains exit option and some voting power in aggregate.
constraint_indexing:constraint_classification(corporate_governance_voting_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CONTROLLING SHAREHOLDER (ROPE) — Experiences voting control as coordination mechanism: governing the firm's strategic direction, maintaining management alignment, capturing board seats. Extraction runs toward this agent through voting mechanisms (supermajority protections, dual-class structures, board stacking). No meaningful exit cost — can arbitrage voting power into governance rights and economic benefit. Net beneficiary.
constraint_indexing:constraint_classification(corporate_governance_voting_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BOARD OF DIRECTORS (PITON) — Formally exercises fiduciary duty and shareholder accountability but functionally exercises nominal review of controlling shareholder decisions. Theater ratio high: independent director committees, audit protocols, say-on-pay votes, and disclosure rules perform governance without substantively constraining controller actions in most cases. Board persists through inertia and regulatory requirement rather than functional constraint mechanism. Degraded from its theoretical coordination function.
constraint_indexing:constraint_classification(corporate_governance_voting_control, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REFORM COALITION (SCAFFOLD) — Organized agents (SEC, institutional investors, proxy advisors, shareholder activism networks) are implementing temporary coordination mechanisms with sunset logic: mandatory proxy access, say-on-pay votes, beneficial ownership disclosure thresholds, and majority-voting standards are building alternative verification pathways for minority shareholder voice. These mechanisms are explicitly designed as transitional — they sunset or phase out as shareholder democracy norms mature or alternative governance models (stakeholder governance, employee voting) replace shareholder primacy. Organized agents have agency and see an exit path.
constraint_indexing:constraint_classification(corporate_governance_voting_control, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, shareholder voting control reflects an immutable economic law: capital concentration and asymmetric information create inherent advantages for controlling shareholders. The constraint appears to be a natural consequence of how property rights operate. However, the structural data contradicts this mountain classification — the engine will compute this as a false summit, revealing that voting control structures are contingent institutional arrangements (dual-class shares, voting agreements, charter provisions) that are designed, not discovered.
constraint_indexing:constraint_classification(corporate_governance_voting_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(corporate_governance_voting_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(corporate_governance_voting_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(corporate_governance_voting_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(corporate_governance_voting_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(corporate_governance_voting_control, TR),
    TR >= 0.70.

:- end_tests(corporate_governance_voting_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The base level (0.35 at T=0) reflects legitimate coordination benefits: voting control does enable strategic decision-making and management alignment. However, extractiveness has increased to 0.58 as control mechanisms have become more sophisticated (dual-class structures, voting agreements, charter-based protections) and as the gap between voting power and economic interest has widened. The metric captures the quantifiable extraction: higher related-party transaction volumes, increased executive compensation relative to firm performance, higher frequency of dilutive issuances, and lower minority shareholder returns (2-5% annual underperformance in high-control firms). Suppression (0.65): High. Multiple barriers constrain minority exit and voice: (1) illiquidity costs of selling sunk positions during normal market conditions; (2) information asymmetries preventing minorities from evaluating whether extraction is occurring; (3) charter and voting agreement barriers to initiating proxy contests (high vote thresholds, staggered boards); (4) coordination costs for dispersed shareholders; (5) reputational risk of challenging the controlling shareholder; (6) regulatory barriers to short-selling (limiting exit for those without ownership). These barriers are structural, not psychological, so suppression is genuine. Theater ratio (0.68): High and increasing. Formal governance mechanisms (proxy contests, say-on-pay votes, board independence requirements, audit committees, disclosure regulations) create the appearance of accountability without substantively constraining controller actions in most cases. Theater has increased over the interval (from 0.42 to 0.68) as regulatory complexity has expanded without corresponding increase in minority voting power. Say-on-pay votes are non-binding; proxy contests have <8% success rates; independent board committees often defer to controller-aligned management.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same voting mechanism appears radically different from different structural positions. The controlling shareholder sees coordination (Rope) — they are solving the legitimate problem of maintaining strategic direction and management accountability to their vision. The board sees nominal accountability (Piton) — they perform governance rituals that satisfy regulatory requirements without constraining controller power. The institutional investor sees mixed coordination and extraction (Tangled Rope) — the system both enables shareholder voice (through coalitions and proxy mechanisms) and constrains them (through information asymmetries and vote thresholds). The regulatory coalition sees a temporary problem with a sunset (Scaffold) — SEC reforms are building alternative pathways (proxy access, majority voting, beneficial ownership disclosure) that will incrementally shift power toward minorities. The minority shareholder sees pure extraction (Snare) — they have voting rights on paper but are mathematically powerless in concentrated firms, subject to dilution and related-party transactions with no exit option. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — voting control reflects immutable property rights and capital concentration dynamics — but the structural data reveals this as a false summit: voting structures are designed institutional arrangements, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position — power level, exit options, and beneficiary/victim status. Controlling shareholders (institutional/arbitrage) have d ≈ 0.05-0.15: they are net beneficiaries with zero exit cost, so f(d) is negative or near-zero, amplifying their effective extraction benefit (low chi means extraction is small from their perspective because the constraint benefits them). Minority shareholders (powerless/trapped) have d ≈ 0.92-0.98: they are victims with trapped exit options, so f(d) is 1.35-1.42, making experienced extraction high (chi = ε × f(d) × σ(S) amplifies the base extractiveness significantly). Institutional investors (moderate/constrained) have d ≈ 0.65-0.72: moderate power and constrained (not trapped) exit, so f(d) ≈ 0.95-1.10, producing moderate effective extraction. The board (institutional/arbitrage) has d ≈ 0.25-0.35: nominally aligned with controlling shareholder (beneficiary), so f(d) ≈ 0.05-0.20, making experienced extraction low from their perspective (but this is misleading — the board's low d reflects that the constraint benefits them, not that extraction is small). The regulatory coalition (organized/constrained) has d ≈ 0.50-0.60: symmetric position trying to reduce control asymmetry, so f(d) ≈ 0.65-0.80, producing moderate experienced extraction relative to their effort to reform the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that different institutional actors have legitimate but incompatible claims about what voting control IS. The controlling shareholder truthfully sees coordination (Rope) — voting control does serve a governance function. The minority shareholder truthfully sees extraction (Snare) — voting control does extract economic benefit beyond proportional ownership. The board truthfully sees degraded ritual (Piton) — formal governance mechanisms are performative. The regulatory coalition truthfully sees temporary scaffolding (Scaffold) — SEC reforms are building alternative pathways. The mandate conflict is not 'which type is correct?' but 'for whom is voting control coordination and for whom is it extraction?' The presheaf over the observation site (the six perspectives) IS the answer. The false summit in the analytical mountain perspective reveals that naturalizing voting control as immutable property rights is an error — the structures are designed and can be redesigned (or replaced by alternative governance models).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voting_control_definition_ambiguity,
    'What fraction of voting control constitutes ''controlling'' status: >50%, >20%, >10%, or the functional threshold below which minority votes matter?',
    'Empirical analysis of proxy contest outcomes, board election patterns, and shareholder proposal success rates at different control thresholds',
    'If threshold = >50%: snare classification applies only to <20% of public companies. If threshold = >20%: applies to ~40%. If functional threshold: applies to ~70% (encompasses all de facto controllers). Classification breadth changes by 3x depending on definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voting_control_definition_ambiguity, empirical, 'Definition of controlling shareholder threshold').

omega_variable(
    voting_power_versus_economic_interest_divergence,
    'How much of the extraction in dual-class voting systems is extractive (controller extracts more economic benefit than their cash-flow ownership justifies) vs coordinative (voting concentration enables efficient governance decisions that benefit the whole firm)?',
    'Long-term performance analysis: companies with concentrated voting vs diffuse voting, controlling for industry and firm size; measurement of related-party transaction magnitude and frequency; analysis of dividend and share buyback patterns relative to cash-flow ownership percentages',
    'If extraction dominates (>60% of control premium): snare/tangled rope classifications are correct. If coordination dominates (<40%): rope classification dominates and suppression metric is overstated. Current estimates range 50-70% extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voting_power_versus_economic_interest_divergence, empirical, 'Extraction vs coordination in dual-class voting structures').

omega_variable(
    proxy_advisor_capture_mechanism,
    'Do proxy advisory firms (ISS, Glass Lewis) actually amplify minority shareholder voice or become a new extraction layer capturing voting power through their own institutional interests?',
    'Analysis of proxy advisor voting recommendations vs minority shareholder interests; identification of conflicts of interest; measurement of whether proxy advisor recommendations shift over time as their parent companies'' interests change; comparison of minority shareholder proposal success rates before/after proxy advisor influence',
    'If proxy advisors amplify voice: scaffold classification confirmed. If proxy advisors capture voting power: institutional investors face a second-order snare (their voting power is mediated by a new institution with its own interests), and the tangled rope classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proxy_advisor_capture_mechanism, empirical, 'Whether proxy advisors amplify or capture minority voice').

omega_variable(
    stakeholder_governance_emergence,
    'Are alternative governance models (employee voting, stakeholder boards, benefit corporation structures) genuinely reducing the functional relevance of shareholder voting control, or are they symbolic additions that leave shareholder primacy intact?',
    'Comparison of shareholder voting power in traditional vs stakeholder-governed firms; measurement of how often stakeholder representatives on boards vote against shareholder-aligned management; analysis of firm outcomes (wage growth, environmental performance, R&D investment) under different governance structures',
    'If genuinely functional: scaffold sunset is real (shareholder voting control becomes less central over time). If symbolic: shareholder voting control remains primary mechanism and scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stakeholder_governance_emergence, empirical, 'Functionality of alternative governance models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(corporate_governance_voting_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cgvc_tr_t0, corporate_governance_voting_control, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cgvc_tr_t20, corporate_governance_voting_control, theater_ratio, 20, 0.58).
narrative_ontology:measurement(cgvc_tr_t40, corporate_governance_voting_control, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(cgvc_be_t0, corporate_governance_voting_control, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cgvc_be_t20, corporate_governance_voting_control, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cgvc_be_t40, corporate_governance_voting_control, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(corporate_governance_voting_control, enforcement_mechanism).
narrative_ontology:affects_constraint(corporate_governance_voting_control, shareholder_value_extraction).
narrative_ontology:affects_constraint(corporate_governance_voting_control, related_party_transaction_approval).

% DUAL FORMULATION NOTE:
% Corporate voting control is upstream of specific extraction mechanisms (shareholder value extraction, related-party transactions, executive compensation). The voting constraint has its own extractiveness reflecting the asymmetric power structure; downstream constraints reflect specific instantiations of that power asymmetry in particular corporate actions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(corporate_governance_voting_control, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
