% ============================================================================
% CONSTRAINT STORY: acquisition_overpayment_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acquisition_overpayment_trap, []).

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
 *   constraint_id: acquisition_overpayment_trap
 *   human_readable: Acquisition Overpayment Trap
 *   domain: corporate_finance/mergers_acquisitions
 *
 * SUMMARY:
 *   The acquisition overpayment trap is a structural constraint where
 *   multiple institutional actors benefit from deal completion at inflated
 *   prices, while the acquirer's shareholders bear the cost of value
 *   destruction. The constraint exhibits genuine coordination functions
 *   (price discovery, deal structuring, due diligence) paired with extractive
 *   mechanisms (fee structures that reward deal completion over synergy
 *   realization, compensation that locks executives into overpayment
 *   incentives, governance theater that permits pricing without rigorous
 *   scrutiny). The constraint is not a natural law of M&A — it is a
 *   contingent institutional arrangement where the alignment of incentives
 *   between deal agents and deal completion overwhelms the alignment between
 *   shareholders and shareholder value. The theater ratio (0.68) reflects
 *   that governance mechanisms (board oversight, proxy advisors, independent
 *   directors) provide procedural legitimacy to pricing decisions without
 *   effectively constraining maximum prices paid. Investment banks present
 *   synergy analyses and fairness opinions; proxy advisors issue buy/sell
 *   recommendations; boards vote approval. The rituals execute, but the
 *   outcomes (systematic overpayment measured post-acquisition) reveal that
 *   the theater is not substantially reducing information asymmetry or price
 *   discovery failure.
 *
 * KEY AGENTS:
 *   - Acquirer Shareholders: Primary victim (powerless/trapped) — locked in by ownership, bear full cost of overpayment through diluted returns
 *   - Target Company Shareholders: Primary beneficiary (institutional/arbitrage) — receive premium, exit via deal acceptance
 *   - Investment Banks & M&A Advisors: Institutional secondary beneficiary (institutional/constrained) — profit from deal fees regardless of long-term acquirer performance
 *   - Acquiring Company Executives: Institutional secondary beneficiary (powerful/constrained) — compensation incentives reward deal completion, career upside from bold acquisitions, downside from withdrawal
 *   - Institutional Investors & Proxy Advisors: Governance actors (organized/constrained) — theoretically provide checks but function as theater through standardized processes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional outcomes as structural information problems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acquisition_overpayment_trap, 0.58).
domain_priors:suppression_score(acquisition_overpayment_trap, 0.65).
domain_priors:theater_ratio(acquisition_overpayment_trap, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acquisition_overpayment_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(acquisition_overpayment_trap, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(acquisition_overpayment_trap, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acquisition_overpayment_trap, tangled_rope).
narrative_ontology:human_readable(acquisition_overpayment_trap, "Acquisition Overpayment Trap").
narrative_ontology:topic_domain(acquisition_overpayment_trap, "corporate_finance/mergers_acquisitions").

domain_priors:requires_active_enforcement(acquisition_overpayment_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acquisition_overpayment_trap, target_company_shareholders).
narrative_ontology:constraint_beneficiary(acquisition_overpayment_trap, investment_banks_advisors).
narrative_ontology:constraint_beneficiary(acquisition_overpayment_trap, executive_compensation_tied_to_deal_completion).
narrative_ontology:constraint_victim(acquisition_overpayment_trap, acquirer_shareholders).
narrative_ontology:constraint_victim(acquisition_overpayment_trap, acquirer_debt_holders).
narrative_ontology:constraint_victim(acquisition_overpayment_trap, future_synergy_realization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ACQUIRER SHAREHOLDER (SNARE) — Locked in by ownership structure. Bears the full cost of overpayment through diluted returns and balance sheet deterioration. No exit without selling shares at depressed prices post-announcement. Maximum extraction experienced.
constraint_indexing:constraint_classification(acquisition_overpayment_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TARGET COMPANY SHAREHOLDERS (ROPE) — Beneficiaries of the premium. Experience the constraint as pure coordination: achieving sale is the mutual problem being solved. Exit easily through accepting the offer. Net benefit — extraction flows toward this agent.
constraint_indexing:constraint_classification(acquisition_overpayment_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INVESTMENT BANKS & ADVISORS (TANGLED ROPE) — Dual role: coordination function (structuring deal, due diligence, facilitating negotiation) AND extraction (fee structure creates incentive to maximize announced deal value, not synergy realization). Can exit by declining representation, but constrained by market competition and reputation damage. Benefits from deal completion regardless of long-term acquirer performance.
constraint_indexing:constraint_classification(acquisition_overpayment_trap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ACQUIRING EXECUTIVE MANAGEMENT (TANGLED ROPE) — Compensation often includes deal-completion bonuses and stock grants that vest post-close. Genuine coordination function (identifying strategic fit, negotiating terms) paired with extraction (incentive to complete deal at any price to trigger bonuses; limited accountability for synergy shortfalls). Constrained by board oversight and shareholder liability, but compensation structure creates misaligned incentives.
constraint_indexing:constraint_classification(acquisition_overpayment_trap, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL INVESTORS & PROXY ADVISORS (PITON) — Theoretically provide governance check on overpayment through voting and engagement. Functionally degraded: proxy advisors offer standardized recommendations (theater); many institutional investors use index-tracking (passive ownership, minimal engagement); anti-takeover defenses limit shareholder ability to reject deals. The governance mechanism persists through institutional inertia but provides minimal actual constraint on deal pricing.
constraint_indexing:constraint_classification(acquisition_overpayment_trap, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From civilizational perspective, overpayment is seen as inherent to M&A: information asymmetry, winner's curse, and synergy overoptimism are structural features of acquisitions. This perspective risks naturalizing what is actually a contingent outcome of deal-specific incentive misalignment and institutional constraints on price discovery. The false summit detector will reveal whether 'structural information problems' are really descriptions of extractive institutional arrangements.
constraint_indexing:constraint_classification(acquisition_overpayment_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acquisition_overpayment_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(acquisition_overpayment_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(acquisition_overpayment_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(acquisition_overpayment_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(acquisition_overpayment_trap, TR),
    TR >= 0.70.

:- end_tests(acquisition_overpayment_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The primary extraction mechanism is the premium paid above fair value for the target company, flowing from acquirer shareholders to target shareholders. The magnitude varies (10-50% in typical deals) and reflects both genuine uncertainty resolution (justified premium for information asymmetry) and unjustified overpayment (cost of executive compensation incentives, advisory fee alignment, pressure to deploy capital). The measurement at 0.58 reflects that overpayment is systematic (documented in academic literature) but not total — some component of acquisition premiums reflects rational pricing of synergies and growth optionality. Suppression (0.65): Moderate-high. Barriers to alternative deal structures and price discovery include: board authority to commit without shareholder vote in many jurisdictions, information asymmetry favoring target's advisors, career incentives that reward bold/large acquisitions over prudence, momentum effects and competitive pressure to not lose targets to other bidders, limited ability for individual shareholders to exit without accepting depressed valuations. These are not absolute barriers, but they create significant friction for resistance. Theater ratio (0.68): High. Deal governance (due diligence, fairness opinions, board votes, proxy proxy recommendations) executes complex procedures that appear rigorous but have limited demonstrated impact on preventing overpayment. The theater increases over time as deals become more complex and advisory ecosystems grow more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The acquiring shareholder and target shareholder occupy opposite extraction positions and experience fundamentally different classifications. The target sees rope (achieving sale is the coordination problem), while the acquirer sees snare (locked in by board decision, bearing the cost). Investment banks see tangled rope (they provide genuine advisory services alongside fee incentives for completion). Executives see tangled rope through the lens of compensation misalignment. Institutional investors see a piton — their governance role is procedurally intact but functionally degraded. The civilizational observer risks a false summit by treating information asymmetry and synergy overestimation as natural laws rather than artifacts of specific institutional designs. The perspectival gap reveals that overpayment is not about knowledge deficits (more analysis would not fix it) but about incentive misalignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Target shareholders benefit from the deal (d ≈ 0.15, arbitrage exit), so they experience negative χ (the constraint benefits them — rope perspective). Acquirer shareholders bear the cost (d ≈ 0.95, trapped exit), experiencing maximum χ (snare perspective). Investment banks and executives occupy intermediate positions with constrained exit and mixed incentives (d ≈ 0.50-0.60, tangled rope perspectives). The derivation from beneficiary/victim status + exit options produces differentiated perspectives without requiring overrides — the structural relationships (who profits, who pays, what exit costs) determine the perspectival gap organically.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy by showing that all six types can be legitimate readings of the same base properties depending on perspective. The acquiring shareholder's snare (extraction with no exit) is not contradicted by the target's rope (coordination problem solved) — both are true simultaneously. The investment bank's tangled rope (genuine advisory service + fee incentive) is not contradicted by the exec's tangled rope (genuine strategic role + compensation lock) — they are different manifestations of the same hybrid mechanism. The institutional investor's piton (governance theater) is not contradicted by the analytical observer's mountain (information-theoretic inevitability) — one reveals contingency, the other risks naturalizing it. The mandatrophy resolves by recognizing that the constraint type depends on which agent's perspective you measure from, and all six perspectives are simultaneously true. The false summit detector should flag the mountain classification as a naturalization risk — the 'inevitable information problem' framing masks contingent institutional choices about how to structure incentives and governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    synergy_estimation_methodology,
    'Are synergy overestimates a structural feature of how humans estimate uncertain benefits, or a predictable artifact of deal-specific incentive misalignment?',
    'Longitudinal comparison of pre-deal synergy forecasts vs post-acquisition realized synergies across 50+ deals. Correlation analysis between deal pricing premium and forecast revision magnitude. Test for systematicity (consistent overestimation) vs noise.',
    'If structural feature: overpayment is inherent, mountain classification justified. If artifact of incentive misalignment: overpayment is extractive design, snare/tangled_rope classification justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(synergy_estimation_methodology, empirical, 'Whether synergy overestimation is structural or incentive-driven').

omega_variable(
    board_veto_threshold,
    'What fraction of proposed acquisitions would be rejected by boards if compensation incentives were removed and synergy forecasts were independently audited?',
    'Randomized audit of synergy forecasts for completed deals by neutral third parties. Estimation of board rejection probability under counterfactual independence. Comparison across acquirer types with different governance structures.',
    'If > 30% rejection probability: board voting is theater, and the suppression of alternative deal structures is real. If < 10%: most deals would survive independent scrutiny, and overpayment is pricing differential rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(board_veto_threshold, empirical, 'What fraction of deals would fail board scrutiny under independent audit').

omega_variable(
    reputational_exit_cost,
    'What is the actual career cost to a CEO who walks away from an acquisition they''ve publicly committed to, versus the market reward for walking away from a deal that would destroy shareholder value?',
    'Historical case studies of deal withdrawals: CEO tenure post-withdrawal, stock price reaction, future compensation. Comparison with completion: long-term CEO tenure, stock price trajectory, reputational outcomes.',
    'If exit cost is very high (CEO departures, reputational damage): exit_options is trapped, not constrained. If markets reward deal withdrawal with higher future valuation and CEO mobility is unchanged: exit cost is low, revealing that continuation is preference misalignment rather than structural lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputational_exit_cost, empirical, 'Career cost of walking away from publicly committed acquisitions').

omega_variable(
    information_asymmetry_scope,
    'How much of the overpayment premium reflects genuine information asymmetry (target knows its prospects better than market), versus asymmetric incentive alignment (acquirer''s agents profit from completion regardless of outcome)?',
    'Analysis of public vs private information gaps: compare target company analyst coverage with information available to acquirer''s advisory team. Test whether premium correlates with information gaps or with deal advisor compensation structure.',
    'If information-driven: overpayment is pricing mechanism, less extractive. If incentive-driven: overpayment is pure extraction mechanism, more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_scope, empirical, 'Information asymmetry vs incentive misalignment in overpayment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acquisition_overpayment_trap, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acqover_theater_t0, acquisition_overpayment_trap, theater_ratio, 0, 0.52).
narrative_ontology:measurement(acqover_theater_t2, acquisition_overpayment_trap, theater_ratio, 2, 0.62).
narrative_ontology:measurement(acqover_theater_t4, acquisition_overpayment_trap, theater_ratio, 4, 0.68).

% Extraction over time
narrative_ontology:measurement(acqover_extract_t0, acquisition_overpayment_trap, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(acqover_extract_t2, acquisition_overpayment_trap, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(acqover_extract_t4, acquisition_overpayment_trap, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acquisition_overpayment_trap, resource_allocation).
narrative_ontology:affects_constraint(acquisition_overpayment_trap, executive_compensation_misalignment).
narrative_ontology:affects_constraint(acquisition_overpayment_trap, investment_bank_fee_structure_incentives).
narrative_ontology:affects_constraint(acquisition_overpayment_trap, board_governance_adequacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
