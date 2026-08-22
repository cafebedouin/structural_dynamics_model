% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Creditor-Discipline Reading of the Gold-Fiat Transition
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   Under Bretton Woods, the dollar's gold convertibility at 35 dollars per
 *   ounce, open to official holders, gave foreign creditors a working veto
 *   over issuer policy: persistent United States deficits drained the gold
 *   stock, and the threat of conversion repeatedly forced restraint (the
 *   1960-61 and 1965-68 contractions are the canonical cases). On 15 August
 *   1971 the window closed; the Smithsonian realignment failed; by 1973 major
 *   currencies floated; the 1976 Jamaica Accords demonetized gold formally.
 *   This reading holds that the transition's essential content was the
 *   elimination of that creditor veto: the reserve issuer acquired the
 *   ability to settle deficits in irredeemable claims, and geopolitical power
 *   moved from creditor nations to the issuer. The constraint this story
 *   classifies is the standing post-transition arrangement as this reading
 *   sees it — a settlement order in which discipline is asymmetric: abolished
 *   for the reserve issuer, retained and in respects tightened for everyone
 *   else through market discipline, IMF conditionality, and the swap-line
 *   hierarchy. Claim and metrics are authored independently: claimed_type
 *   records this reading's structural judgment (genuine liquidity
 *   coordination plus asymmetric transfer plus active enforcement); the
 *   metrics describe observed operation. Domestic distributional incidence
 *   inside the issuer is a separate constraint and is deliberately not folded
 *   in here.
 *
 * KEY AGENTS:
 *   - - us_reserve_currency_issuer: Agenda-setter and principal beneficiary (institutional/arbitrage) — issues the settlement asset, collects seigniorage, sets terms others accept
 *   - - surplus_creditor_nations: Primary target (powerful/trapped) — lost redemption leverage; hold irredeemable claims whose real value the issuer controls
 *   - - deficit_debtor_nations: Dual-positioned beneficiary/payer (moderate/constrained) — gained fiscal flexibility versus the gold era, absorbed tightened market discipline
 *   - - petrostate_dollar_recyclers: Secondary beneficiary (powerful/constrained) — accept paper for oil under the issuer's security umbrella
 *   - - rival_reserve_aspirants: Excluded party (powerful/trapped) — would supply alternative reserve assets; kept marginal by network effects
 *   - - imf_surveillance_apparatus: Institutional observer (institutional/analytical) — administers adjustment asymmetrically across members
 *   - - monetary_history_analysts: Analytical observer (analytical/analytical) — reconstructs the transition from archives and data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.72).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.64).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Creditor-Discipline Reading of the Gold-Fiat Transition").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "economic/political/historical").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '89fe532d-a5de-43ef-82df-5095abd92f96').
narrative_ontology:cs_kernel_codification('89fe532d-a5de-43ef-82df-5095abd92f96', distributed).
narrative_ontology:cs_authority_grounding('89fe532d-a5de-43ef-82df-5095abd92f96', distributed).
narrative_ontology:cs_reading_relation('89fe532d-a5de-43ef-82df-5095abd92f96', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('89fe532d-a5de-43ef-82df-5095abd92f96', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('89fe532d-a5de-43ef-82df-5095abd92f96', foundational, creditor_redemption_rights_are_load_bearing_discipline).
narrative_ontology:cs_axiom_status(creditor_redemption_rights_are_load_bearing_discipline, holdable).
narrative_ontology:cs_axiom_grounding('89fe532d-a5de-43ef-82df-5095abd92f96', creditor_redemption_rights_are_load_bearing_discipline, empirically_contingent).
narrative_ontology:cs_axiom('89fe532d-a5de-43ef-82df-5095abd92f96', foundational, unilateral_repudiation_transfers_sovereignty_to_issuer).
narrative_ontology:cs_axiom_status(unilateral_repudiation_transfers_sovereignty_to_issuer, holdable).
narrative_ontology:cs_axiom_grounding('89fe532d-a5de-43ef-82df-5095abd92f96', unilateral_repudiation_transfers_sovereignty_to_issuer, deontological).
narrative_ontology:cs_reference_frame('89fe532d-a5de-43ef-82df-5095abd92f96', bretton_woods_creditor_veto_order).
narrative_ontology:cs_drift_state('89fe532d-a5de-43ef-82df-5095abd92f96', contemporary_fiat_dollar_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('89fe532d-a5de-43ef-82df-5095abd92f96', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, deficit_debtor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, surplus_creditor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, petrostate_dollar_recyclers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, deficit_debtor_nations).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, exorbitant_privilege_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's primary settlement and reserve asset. Sets its own monetary policy with no external redemption claims standing against it, and finances fiscal deficits by selling claims that foreign official institutions hold as reserves. Collects seigniorage and borrows more cheaply and deeply than any other sovereign. Leaving this position would mean deliberately dismantling the demand structure its own liabilities rest on.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_currency_issuer, beneficiary).

% Run persistent trade surpluses and accumulate the issuer's claims as reserves. Before 1971 they could present those claims for gold, which gave them leverage over issuer policy; since the window closed, the claims carry no redemption right and their real value depends on policy the holders do not control. Holding vast stocks of another sovereign's unfunded paper, their alternatives — gold, other currencies, bilateral settlement — lack the depth and acceptance of the incumbent asset, and rapid divestment would devalue their own remaining holdings.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, surplus_creditor_nations, payer,
    powerful, generational, trapped, global).

% Import capital and run current-account deficits. Freed from the gold era's requirement to deflate or devalue on schedule, they borrow in the reserve asset; but without reserve-currency status they face market discipline the issuer escapes — sudden stops, IMF conditionality, and repayment in a currency they must earn through exports. Flexibility and tightened discipline arrive together.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, deficit_debtor_nations, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, deficit_debtor_nations, payer).

% Price oil exports in the reserve asset and recycle the proceeds into the issuer's government bond markets under security arrangements with the issuer. They receive access to deep liquid markets and protection guarantees; they also accept paper claims in exchange for finite resources and bear the inflation risk on those claims.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, petrostate_dollar_recyclers, beneficiary,
    powerful, biographical, constrained, regional).

% Would issue or promote alternative reserve and settlement assets — a unified European currency, later internationalized alternatives. Invoicing inertia, network effects, and the incumbent's alliance structure keep their assets marginal; building rival infrastructure takes decades and draws friction from the incumbent.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, rival_reserve_aspirants, excluded,
    powerful, generational, trapped, continental).

% Monitors member balance of payments and lends into crises under conditionality. After the 1976 Jamaica amendments it administers a system in which the largest member is effectively exempt from the adjustment pressures applied to all others.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_surveillance_apparatus, observer,
    institutional, generational, analytical, global).

% Reconstruct the transition from archives, memoirs, and balance-of-payments data, and publish competing accounts of what changed in August 1971 and why. Hold no stake in the arrangement's continuation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, monetary_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, us_reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies elastic international liquidity and a universally acceptable settlement and reserve asset: expanding world trade needs expanding reserves, and a fiat reserve asset grows with demand instead of forcing deflationary adjustment onto deficit countries, as metal anchors periodically did.
% TRANSFER_FUNCTION: Moves real resources and claim-services from surplus and creditor nations to the reserve issuer: exporters ship goods, official holders accept irredeemable claims, and the issuer finances deficits and collects seigniorage; inflation and valuation losses on reserve portfolios return part of the cost to the holders.
% ABSENT_VOICES: Creditor-nation finance ministries and central banks held a formal seat before 1971 through gold convertibility and lost it when the window closed; rival reserve issuers and hard-money advocates object from outside the governance core — in academic journals, IMF minority positions, and occasional diplomatic protest rather than decision channels.
% DISAPPEARANCE_RATIONALE: If the fiat-dollar settlement order vanished overnight, trade invoicing, reserve portfolios, Treasury funding, and petrodollar recycling would all reorganize immediately; the issuer would lose its deficit-financing channel and creditors would suddenly hold claims on a system that no longer exists.
% FOUNDING_PROBLEM: How to supply enough international liquidity for growing trade without a metal anchor whose supply is inelastic — the Triffin problem: the reserve issuer must run deficits to supply reserves, which progressively undermines confidence in its convertibility.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: IMF and BIS research programs on global safe-asset scarcity, the academic revival of the Triffin problem (Gourinchas-Rey valuation-channel work, Obstfeld), and central-bank reserve-manager surveys reporting unmet demand for sovereign risk-free assets. Creditor-nation officials independently attest the cost side. No party outside the issuer attests that the current arrangement is the only available solution.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the reading locates a large, compounding transfer: seigniorage on outstanding official holdings, the 1970s inflation that roughly halved the real value of foreign official dollar claims precisely when redemption was barred, and the option value of deficit finance without adjustment. Suppression (0.64) is structural rather than violent — network lock-in through invoicing and Treasury-market depth, plus active machinery (legal tender standing, historical capital controls, petrodollar diplomacy, and later sanctions infrastructure built on the payment rails); it is authored as a raw structural property and left unscaled, since only extractiveness is scaled by directionality and scope in the engine's computation. Theater ratio (0.34) reflects a real but increasingly rhetorical maintenance layer: G7 communiques, strong-dollar statements, and surveillance exercises whose share of total activity grows as the substantive bargain recedes into habit. Accessibility collapse (0.52): alternatives exist (gold, SDRs, other currencies) but none reaches the incumbent's network scale, and understanding the lock-in does not dissolve it. Resistance (0.48): French conversion campaigns before 1971, SDR advocacy, reserve diversification, regional swap arrangements, and periodic de-dollarization discourse — real, recurring, and fragmented. The three temporal series run on one shared eight-point grid (1958-2024) so every tracked metric is authored at every examined time point; trajectories are monotonic rather than cyclical, driven by accumulating asymmetry rather than oscillating enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the agenda-setter seat compute differently from the same structure. From the issuer's position the arrangement is a public good it provides — lender of last resort, deep safe assets, elastic liquidity — and the absence of redemption claims is simply what modern money is. From the creditor seat the same facts read as uncompensated exposure: a counterparty that repudiated convertibility unilaterally, sets the real return on your reserves, and finances its deficits with your surpluses. Same-level divergence among creditors matters too: Gulf creditors are partly compensated through security rents, while East Asian creditors bear more of the transfer net. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The issuer sits near the full-beneficiary end: it declares the terms, collects the seigniorage, and holds arbitrage-grade position (its liabilities are everyone else's necessity). Surplus creditor nations sit near the full-target end: they bear the transfer, and trapped exit amplifies their effective burden — the derivation from victim declaration plus trapped exit needs no override. Deficit debtor nations derive mid-range: genuine flexibility gains (beneficiary declaration) offset by tightened discipline for non-reserve holders (secondary payer position). Petrostate recyclers derive low-to-mid: they are paid in the extracted asset but compensated through security guarantees and market depth. Rival aspirants are excluded rather than positioned — their exclusion is part of what the enforcement machinery maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents two symmetrical mislabels. Calling the arrangement a snare would erase the coordination function this reading itself concedes: elastic fiat liquidity solved a real problem the gold standard could not, and deficit nations did gain flexibility the metal anchor denied them. Calling it a rope would erase the unilateral 1971 repudiation and the compounding transfer the measurement series records. Mandatrophy is not resolved: the founding problem (supplying elastic world liquidity) remains live per the R5 interview, so the arrangement has not outlived its function — it has redistributed its costs. A dead-problem-plus-world-rearranges mismatch, the zombie signature, is absent here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_assignment,
    'Is the post-1971 arrangement''s operative content the elimination of creditor veto power (this reading), the substitution of institutional discretion for a material constraint (automatic_constraint_reading), or the convergence of multiple independent structural changes (composite_overdetermination_reading)?',
    'Comparative structural analysis: test which reading best predicts post-1971 issuer behavior and distributional outcomes — whether policy changes track the loss of redemption exposure, the arrival of discretionary tools, or the timing of telecommunications and labor shifts.',
    'This reading yields a high-epsilon tangled_rope keyed to the creditor-debtor asymmetry; the automatic reading would key classification to rule-versus-discretion and likely lower epsilon; the composite reading would distribute epsilon across converging mechanisms and reduce the Nixon Shock''s causal weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_assignment, conceptual, 'Kernel-level contest over which structural element defines the transition; this file authors one reading only.').

omega_variable(
    seigniorage_transfer_magnitude,
    'How large is the ongoing real transfer from official creditors to the reserve issuer — the exorbitant-privilege component of measured extractiveness?',
    'Valuation-channel accounting in the Gourinchas-Rey tradition, archival reserve-composition data, and cross-country comparison of sovereign financing costs at matched risk.',
    'Estimates spanning roughly 0.3 to over 1 percent of issuer GDP annually straddle the rope-snare boundary: a small verified transfer supports a coordination-cost reading; a large one supports treating the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_transfer_magnitude, empirical, 'Magnitude of the seigniorage and valuation transfer underlying the epsilon score.').

omega_variable(
    creditor_exit_feasibility,
    'Are surplus creditor nations genuinely trapped, or does gradual diversification — gold accumulation, bilateral settlement channels, alternative payment infrastructure — make exit feasible at tolerable cost?',
    'Track realized reserve reallocation, invoicing-currency shares, and the market impact of large disclosed portfolio shifts over successive decades.',
    'If exit is feasible, the trapped modifier overstates effective extraction for the creditor seat and the computed classification drifts toward rope; if exit remains blocked, the current high-directionality reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_exit_feasibility, empirical, 'Whether the creditor seat''s exit options are truly trapped or merely constrained.').

omega_variable(
    counterfactual_discipline_binding,
    'Would continued gold convertibility actually have restrained issuer deficits, or was the discipline already dead — a Triffin-infeasible arrangement that would have collapsed under any policy?',
    'Counterfactual analysis of the 1960s gold-drain episodes: whether credible restraint paths existed that preserved convertibility, using archival policy deliberations and reserve-flow modeling.',
    'If the constraint was already non-binding, the transition removed nothing load-bearing and epsilon attribution shifts toward the composite reading; if it was binding, this reading''s premise that a real veto was destroyed is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_discipline_binding, conceptual, 'Whether the eliminated creditor veto was load-bearing discipline or a dying formality.').

omega_variable(
    nonreserve_debtor_net_position,
    'Did non-reserve debtor nations gain net fiscal flexibility or suffer net tightening relative to the Bretton Woods adjustable-peg era?',
    'Compare crisis frequency, conditionality incidence, and borrowing-cost volatility for non-reserve deficit countries across the pegged and floating eras.',
    'The declared structure asserts both effects simultaneously (eliminated for the reserve issuer, tightened for non-reserve holders); resolving the net sign determines whether the deficit_debtor_nations seat computes as net beneficiary or net payer, moving its directionality materially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nonreserve_debtor_net_position, conceptual, 'Sign ambiguity in the dual-positioned debtor seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1958, 0.1).
narrative_ontology:measurement_basis(gold_tr_t1958, observed).
narrative_ontology:measurement(gold_tr_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement_basis(gold_tr_t1965, observed).
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement_basis(gold_tr_t1980, observed).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(gold_tr_t1990, observed).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(gold_tr_t2000, observed).
narrative_ontology:measurement(gold_tr_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement_basis(gold_tr_t2010, observed).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 2024, 0.34).
narrative_ontology:measurement_basis(gold_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1958, 0.3).
narrative_ontology:measurement_basis(gold_be_t1958, observed).
narrative_ontology:measurement(gold_be_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement_basis(gold_be_t1965, observed).
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1971, 0.52).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement_basis(gold_be_t1980, observed).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 1990, 0.63).
narrative_ontology:measurement_basis(gold_be_t1990, observed).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2000, 0.66).
narrative_ontology:measurement_basis(gold_be_t2000, observed).
narrative_ontology:measurement(gold_be_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement_basis(gold_be_t2010, observed).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 2024, 0.72).
narrative_ontology:measurement_basis(gold_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1958, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement_basis(gold_su_t1958, observed).
narrative_ontology:measurement(gold_su_t1965, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement_basis(gold_su_t1965, observed).
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1971, 0.5).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement_basis(gold_su_t1980, observed).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement_basis(gold_su_t1990, observed).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2000, 0.57).
narrative_ontology:measurement_basis(gold_su_t2000, observed).
narrative_ontology:measurement(gold_su_t2010, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement_basis(gold_su_t2010, observed).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 2024, 0.64).
narrative_ontology:measurement_basis(gold_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, global_infrastructure).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the gold-fiat transition' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into a three-story family sharing the kernel gold_fiat_transition_mechanism. The automatic_constraint_reading is the mechanistic baseline (what kind of constraint changed: material to institutional); this creditor_discipline_reading is the distributional superstructure (who gained and lost: creditors to issuer) and depends on the baseline's account of what the old constraint physically did; the composite_overdetermination_reading is a rival causal topology that redistributes explanatory weight across technology, peg collapse, labor shifts, and legal-tender maturation. Each story carries its own epsilon, beneficiary/victim structure, and claimed type; all three are linked through affects_constraints so contamination and drift propagate visibly across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
