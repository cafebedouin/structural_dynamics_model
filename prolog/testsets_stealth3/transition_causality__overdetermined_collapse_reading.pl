% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Triffin Overdetermination Bind on the Bretton Woods Parity Regime
 *   domain: monetary economics/political economy/international finance
 *
 * SUMMARY:
 *   The Bretton Woods fixed-rate gold-exchange standard (operational from
 *   1944, fully convertible from 1958, terminated by the suspension of
 *   dollar-gold convertibility in August 1971) rested on a contradiction its
 *   own chief architect's successors diagnosed early: supplying the world
 *   economy with reserve liquidity required the United States to run
 *   persistent external deficits, and those deficits progressively stripped
 *   the gold backing from the convertibility pledge every parity depended on.
 *   This file instantiates ONE reading of the resulting causality dispute —
 *   the overdetermined_collapse_reading — which holds that the end was
 *   structurally inevitable: multiple reinforcing contradictions
 *   (liquidity-versus-confidence arithmetic, adjustment asymmetry, the
 *   fiscal-monetary expansion of the 1960s, the mechanics of speculation
 *   against a known-fragile anchor) converged on the same outcome regardless
 *   of who governed or what they tried. Assumptions stated explicitly: the
 *   interval is the regime's operational life (1944-1971); scalar base
 *   properties follow the terminal-state convention (final grid point)
 *   because the reading weights the mature, contradiction-laden phase;
 *   epsilon's referent is the standing fixed-rate arrangement as THIS reading
 *   assesses it — mounting uncompensated cost-bearing with decaying
 *   coordination return — and never the floating successor regime this
 *   reading did not endorse. KEY AGENTS (by structural relationship): -
 *   us_treasury_fed_authorities: Administrator and most exposed party
 *   (institutional/arbitrage) — ran the defense operations, issued the
 *   reserve asset, held the only unilateral exit -
 *   imf_par_value_administrators: Co-administrator without control of the
 *   anchor (institutional/constrained) — administered parities and
 *   conditionality it could not redesign -
 *   foreign_central_bank_reserve_holders: Primary bearer of costs
 *   (organized/trapped) — absorbed reserve erosion with redemption rationed
 *   by the issuer - nonreserve_deficit_countries: Cost-bearers under
 *   adjustment discipline (powerful/constrained) -
 *   surplus_countries_accumulating_dollars: Cost-bearers on the inflow side,
 *   incidental gainers on the export side (powerful/constrained) -
 *   private_cross_border_finance: Lateral contrast seat (organized/arbitrage)
 *   — paid on unhedged parity breaks, profited by routing around controls -
 *   monetary_historians: Analytical observer — sees the full structure from
 *   outside the arrangement
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.74).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.8).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.86).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Triffin Overdetermination Bind on the Bretton Woods Parity Regime").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary economics/political economy/international finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '376ae580-e064-4f09-ae38-17d5be8f7726').
narrative_ontology:cs_kernel_codification('376ae580-e064-4f09-ae38-17d5be8f7726', distributed).
narrative_ontology:cs_authority_grounding('376ae580-e064-4f09-ae38-17d5be8f7726', expertise).
narrative_ontology:cs_interpretation_layer_present('376ae580-e064-4f09-ae38-17d5be8f7726').
narrative_ontology:cs_reading_relation('376ae580-e064-4f09-ae38-17d5be8f7726', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('376ae580-e064-4f09-ae38-17d5be8f7726', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('376ae580-e064-4f09-ae38-17d5be8f7726', foundational, structural_contradictions_determine_outcome_independent_of_policy).
narrative_ontology:cs_axiom_status(structural_contradictions_determine_outcome_independent_of_policy, holdable).
narrative_ontology:cs_axiom_grounding('376ae580-e064-4f09-ae38-17d5be8f7726', structural_contradictions_determine_outcome_independent_of_policy, empirically_contingent).
narrative_ontology:cs_axiom('376ae580-e064-4f09-ae38-17d5be8f7726', secondary, converging_failure_pathways_leave_no_stabilization_interior).
narrative_ontology:cs_axiom_status(converging_failure_pathways_leave_no_stabilization_interior, holdable).
narrative_ontology:cs_axiom_grounding('376ae580-e064-4f09-ae38-17d5be8f7726', converging_failure_pathways_leave_no_stabilization_interior, empirically_contingent).
narrative_ontology:cs_reference_frame('376ae580-e064-4f09-ae38-17d5be8f7726', structural_contradiction_baseline).
narrative_ontology:cs_drift_state('376ae580-e064-4f09-ae38-17d5be8f7726', contemporary_post_archival_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('376ae580-e064-4f09-ae38-17d5be8f7726', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, foreign_central_bank_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, nonreserve_deficit_countries).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, surplus_countries_accumulating_dollars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, surplus_countries_accumulating_dollars).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, private_cross_border_finance).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, us_treasury_fed_authorities).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, private_cross_border_finance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administered the dollar's gold-convertibility pledge and led every defense operation: gold sales through the London pool, reciprocal swap lines, the interest equalization levy, voluntary restraint programs on lending abroad. Issuing the world's principal reserve asset let US residents and the federal government acquire foreign goods, assets, and basing rights by creating dollar claims others were obliged to hold. By the mid-1960s the same issuance that supplied the system was draining the metal stock behind the pledge; defending the pledge domestically demanded tightening severe enough to produce recession, while easing accelerated the drain. Held the one unilateral exit in the system — suspending conversion, taken in August 1971, which dissolved the bind by dissolving everyone else's anchor.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_treasury_fed_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, us_treasury_fed_authorities, payer).

% Ran the par-value machinery: approved parity changes, extended conditional credit to deficit members, hosted the negotiations that produced substitute reserve assets. Could not touch the anchor itself — the Articles tied the unit's value to metal held in Washington, and consequential decisions required the consent of the very member whose currency stood under strain. Shepherded special drawing rights into existence in 1969 after nearly a decade of negotiation; the allocation proved too small and too slow to relieve the pressures building on the dollar. Had no path out of its own charter short of the system's end.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, imf_par_value_administrators, agenda_setter,
    institutional, generational, constrained, global).

% Accumulated dollar balances as the counterpart of intervening to hold announced parities. Converting those balances for metal was rationed by the issuer's willingness and by reciprocity understandings, so holdings compounded faster than the backing behind them. Selling dollars openly would depress the value of their own remaining reserves; converting at scale would break the arrangement they were charged with upholding. Held position year after year, absorbing erosion, while pressing privately for parity changes and a higher gold price.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, foreign_central_bank_reserve_holders, payer,
    organized, biographical, trapped, continental).

% Ran external deficits under fixed parities and financed them through standby credits carrying conditions on domestic demand, wages, and public spending. Devaluation was the designated remedy, but each use invited speculative attack on the next parity, so adjustment arrived chiefly as stop-go austerity. The United Kingdom's sequence of crises, standby agreements, and the 1967 devaluation is the pattern case; smaller deficit members faced the same discipline with less bargaining weight.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, nonreserve_deficit_countries, payer,
    powerful, biographical, constrained, regional).

% Bought dollars continuously to hold their currencies at announced parities, accumulating reserve claims as the mirror image of others' deficits. The inflows expanded their money supplies beyond domestic preference, forcing sterilization operations or accepting imported inflation; declining to absorb risked appreciation their export industries opposed. Exporters concurrently gained assured access to the large American market under stable rates. Revaluation remained legally available throughout, but each unilateral move carried diplomatic cost and competitive fear, and the Federal Republic's 1969 revaluation came only after prolonged resistance.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, surplus_countries_accumulating_dollars, payer,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, surplus_countries_accumulating_dollars, beneficiary).

% Moved funds ahead of parity changes and intermediated borrowing outside national regulations, building offshore dollar markets that routed around domestic controls. Captured the spreads between official prices and market expectations, and earned margins the controls existed to prevent. On the other side, unhedged positions took heavy losses when parities moved in 1969, 1971, and 1973; insurance against official prices advertised as immutable was priced as unnecessary.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, private_cross_border_finance, payer,
    organized, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, private_cross_border_finance, beneficiary).

% Reconstruct the decision record from opened archives, test counterfactuals against the quantitative series, and adjudicate among competing causal accounts of the system's end. Collect and pay nothing in the arrangement itself; their findings feed textbooks, policy curricula, and the design debates of successor monetary institutions.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixed-but-adjustable parities anchored by a gold-convertible dollar gave traders and lenders predictable exchange rates, gave deficit members a disciplined adjustment path, and supplied world liquidity through the issuer's balance-of-payments deficits — solving centrally, and for the first time, the joint liquidity-and-anchor problem the interwar system never solved.
% TRANSFER_FUNCTION: Moved real resources toward the issuer in exchange for newly created dollar claims (financing external deficits and overseas commitments); moved reserve erosion onto foreign central banks holding dollar balances; moved adjustment burdens — austerity and devaluation — onto deficit countries; after August 1971, moved devaluation losses onto every holder of dollar claims as the anchor's promise lapsed.
% ABSENT_VOICES: Wage earners in adjusting deficit countries bore the austerity without a seat at the Basel meetings or the Fund board where conditions were set; colonies and newly independent states had no vote in the 1944 design and marginal voice in drawing-rights allocation; electorates on both sides of the Atlantic never ratified the convertibility pledge their savings and wages backed. They sit outside the negotiating rooms where parity changes and reserve-asset design were decided.
% DISAPPEARANCE_RATIONALE: Without the bind, the parity system has no internal dissolution mechanism: parities hold, the gold window never closes, and the 1971-1973 reordering — generalized floating, the demonetization of gold, the Fund's reinvention from parity-warden to surveillance body — never occurs. Every successor arrangement, from floating regimes to regional currency blocs to the modern reserve system, sits downstream of the transition the bind forced.
% FOUNDING_PROBLEM: Interwar monetary nationalism: competitive devaluations, inconvertible currencies, rival currency blocs, and a world liquidity shortage that deepened the slump — the 1944 design sought stable but adjustable parities anchored in a gold-convertible dollar.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by the British delegation's negotiation records — Keynes's clearing-union memoranda sought the symmetric adjustment the final design declined, an independent witness to what problem the conference convened to solve — by interwar-comparative scholarship published independently of any participant government, and by Fund-external economic history. The US Treasury's own retrospective self-attestation is discounted accordingly.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises monotonically across the grid (0.16 to 0.74) as the arrangement shifted from delivering scarce liquidity (dollar-shortage 1940s) to imposing uncompensated cost-bearing on every seat (dollar-glut 1960s): reserve erosion for holders, stop-go austerity for deficit countries, imported inflation for surplus countries, recession-inducing defense tightening for the issuer's own economy. Suppression_requirement dips at 1950 (high confidence, light enforcement) then ratchets to 0.80 as defense of parities demanded ever-heavier machinery — the Interest Equalization Tax, voluntary restraint programs, swap networks, conditionality, finally the gold window's closure — each layer buying time, none changing the outcome; this series tracks enforcement-capacity buildup specifically, which is the dynamic the story traces. Theater_ratio climbs from 0.10 to 0.70 as proxy activity replaced function: communiques reaffirming parities under strain, the two-tier gold fiction of 1968 (official $35 upheld beside a free market price it no longer governed), culminating in the December 1971 realignment celebrated as historic while its central parity survived fourteen months. The trajectory is a monotonic ratchet, not a cycle: no reconciliation phase resets the tension, because the driving variable (cumulative reserve claims against finite gold) moved one way. Accessibility_collapse is high (0.86): once the liquidity-confidence arithmetic is understood, the interior of the policy space closes — every stabilization path (Gold Pool, SDRs, Smithsonian) is visible in hindsight as motion along the same descending gradient, which is the reading's core claim rendered as a measurement. Resistance is high (0.62) but zero-traction: French gold conversion policy, speculative runs, unilateral revaluations, the issuer's own defense programs — massive active resistance with no effect on the outcome. Voluminous ineffective resistance against a constraint is itself the mountain signature: the constraint does not need the resistance to fail. The claim (mountain) and these metrics are independently authored: the metrics describe the terminal turbulence of a dying arrangement; the claim describes the arithmetic underneath it. The engine computes per-seat types and may diverge; that divergence is the measurement.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the administrator seat (us_treasury_fed_authorities), the arrangement presents as an impossible management problem: every available instrument trades one catastrophic cost for another, and the only clean exit is one no steward could take lightly. From the trapped reserve-holder seat, the same structure presents as gradual confiscation — obligations to accumulate claims whose backing visibly shrank, with redemption rationed by the debtor. From the constrained sovereign seats, it presents as surrendered macroeconomic autonomy under external discipline. From the arbitrage seat (private_cross_border_finance), it presents as opportunity: rigid official prices guarantee someone a spread. The analytical seat sees the convergence of pathways the participants each experienced locally. Inter-institutionally, the two agenda-setters differ sharply: the US administration controlled the anchor and thus held arbitrage-grade exit (suspend, and the bind dissolves by destroying everyone else's reference point), while the Fund administered parities it could not redefine and exited nowhere — same nominal institutional tier, opposite exit structures, purely because of where the anchor sat. Same-level lateral divergence: central banks and private cross-border finance both held 'organized' power facing identical official prices, yet the former were locked by mandate and market-impact fear while the latter routed around controls through offshore markets — exit differentiation driven entirely by constraint-specific factors, not by global standing.
 *
 * DIRECTIONALITY LOGIC:
 *   No seat collects from the bind itself — this is the load-bearing directionality fact. Seigniorage accrued to the issuer from the arrangement's liquidity function, not from the contradiction; the contradiction then taxed that privilege away (the US gold stock fell roughly sixty percent defending the pledge it financed), which is why gain_flow is authored 'diffuse' as an affirmative checked claim: the administrator's inflow was offset and exceeded by defense costs, reserve holders net-lost, deficit countries paid in austerity, surplus countries paid in imported inflation against partial export gains, private finance was two-sided. Beneficiary/victim declarations map to real positions: the three declared victim groups correspond to stakeholders carrying role payer; the administrators carry agenda_setter (with the US seat genuinely dual-positioned as secondary payer — it bore the bind's sharpest point). Effective extraction is amplified for the trapped targets (redemption rationed, holdings immobile) and for all seats by the arrangement's global scope, which raised verification difficulty and made coordinated defense the only defense. Suppression is authored as the raw structural property it is — the coercive machinery of parity defense — and is not scaled by any context dimension; only extractiveness rides directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar monetary chaos: competitive devaluations, inconvertible blocs, world liquidity shortage — was substantially solved by the late 1950s, when the dollar shortage flipped to a dollar glut. The arrangement then persisted fourteen more years on path dependence and Cold War finance, which is why founding_problem_status is authored dead alongside disappearance_verdict world_rearranges: the mismatch flag the consumer computes from that pair is expected and correct, and the cross-check against the theater path confirms it — but with a twist the classification exists to catch. A dead mandate plus world-rearranging dependence normally indicates a calcified vestige drifting on inertia. Here the arrangement never got the chance to calcify: the bind killed it outright. Enforcement intensity rose (0.22 to 0.80) while effectiveness fell to zero — the signature of fighting arithmetic, not of harvesting rents or coasting on habit. Reading the terminal phase as pure extraction (a coercion story) would misattribute the turbulence to someone's strategy; reading it as inertial vestige (a drift story) would mispredict persistence that never occurred. The correct reading is neither: a structural limit operating through a constructed envelope, terminating the envelope rather than outliving its function inside it. The classification's job is to keep those three failure modes — snare, piton, mountain-through-institution — from being confused, because they call for entirely different counterfactual questions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_bind,
    'Is the bind irreducible structural reality (a limit that holds for any actor inside the configuration, with zero degrees of freedom), or an artifact of one particular design — a national fiduciary currency serving as world reserve under fixed parities and a gold anchor — such that a different architecture would have dissolved it?',
    'Comparative institutional analysis: search the archival record for redesign proposals with a credible adoption path (Keynes''s clearing union and symmetric adjustment scheme rejected at Bretton Woods; SDR substitution-account proposals of the 1970s never activated) and test whether any preserved the coordination function without the liquidity-confidence contradiction.',
    'If a viable redesign existed and was declined for distributional reasons, part of the inevitability is constructed rather than structural; classification drifts toward a designed-cost-sharing arrangement with identifiable designers on the gaining side, and the inevitability claim weakens relative to the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_bind, empirical, 'Whether the structural bind is a natural limit or a consequence of the chosen reserve architecture.').

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the kernel transition_causality: the overdetermined_collapse_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Archival counterfactual reconstruction plus quantitative stress-testing of the regime under alternative policy mixes (earlier dollar devaluation, SDR-scale substitution, capital-control harmonization). Resolution assigns a probability to the counterfactual ''an alternative policy path sustains the regime'': near zero for this reading, positive for the contingent_choice_reading, conditional-on-triggers for the hybrid_trigger_reading.',
    'Resolution toward viability collapses the mountain character of this reading (the arrangement becomes a repairable coordination device; type drifts toward transitional-support categories and epsilon falls). Resolution toward inevitability strengthens the foreclosure edge this reading holds against the contingent_choice_reading. The siblings are separate constraint files with their own epsilon, beneficiary structure, and classification; nothing here averages over them.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, and where the dispute sits (counterfactual viability of alternative policy paths).').

omega_variable(
    multiple_pathways_vs_single_mechanism,
    'Did multiple independent failure mechanisms genuinely converge (US monetary expansion, gold-cover erosion, speculation elasticity, asymmetric adjustment burden), or was collapse driven by one dominant channel with the others epiphenomenal?',
    'Variance decomposition of collapse-timing models across candidate channels, using reserve-ratio series, US inflation differentials, gold-stock data, and speculation-flow elasticities.',
    'A single-dominant-channel finding would push this reading toward the hybrid account (one mechanism plus trigger timing) and dissolve the convergence axiom; confirmation of multiple independent channels is the load-bearing evidence for the overdetermination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multiple_pathways_vs_single_mechanism, empirical, 'Whether the causal pathways were genuinely plural and converging.').

omega_variable(
    victim_boundary_full_cycle_accounting,
    'Over the full 1944-1971 cycle, which seats are net victims of the fixed-rate arrangement — do surplus-country exporters and early-phase seigniorage recipients reverse sign in the terminal phase, and does ''all actors constrained by the regime'' hold as a net or only a terminal-phase statement?',
    'Full-cycle welfare accounting per seat: seigniorage gains versus gold-drain defense costs for the issuer; reserve erosion versus export-market stability for surplus countries; austerity incidence for deficit countries.',
    'A narrower net-victim set thins the victim structure and lowers effective pressure on the payer seats; a broader confirmed set supports reading the whole regime as a cost-distributing arrangement even though the underlying bind itself collects from no one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_boundary_full_cycle_accounting, empirical, 'Boundary of the victim structure: net full-cycle versus terminal-phase victimhood per seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__overdetermined_collapse_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.41).
narrative_ontology:measurement(tran_tr_t1968, transition_causality__overdetermined_collapse_reading, theater_ratio, 1968, 0.56).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.7).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.16).
narrative_ontology:measurement(tran_be_t1950, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1950, 0.2).
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.33).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.51).
narrative_ontology:measurement(tran_be_t1968, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1944, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1944, 0.28).
narrative_ontology:measurement(tran_su_t1950, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1950, 0.22).
narrative_ontology:measurement(tran_su_t1958, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1958, 0.36).
narrative_ontology:measurement(tran_su_t1965, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1965, 0.49).
narrative_ontology:measurement(tran_su_t1968, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1968, 0.61).
narrative_ontology:measurement(tran_su_t1971, transition_causality__overdetermined_collapse_reading, suppression_requirement, 1971, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'why Bretton Woods ended' conflates three structurally distinct constraints, each with its own epsilon, victim structure, and classification. This file (overdetermined_collapse_reading) carries the strongest causal claim — inevitability from converging structural contradictions — and functions as the upstream node whose premise strength conditions the others: the hybrid reading accepts the structural accumulation and disputes only activation, while the contingent reading denies the structural determination outright. The epsilon values differ by reading because each assesses the same standing arrangement (the fixed-rate parity regime) by its own lights: this reading finds the arrangement's cost-bearing uncompensated and its coordination return decaying (high epsilon); a contingent reading that sees a salvageable arrangement authors lower extraction attributable to remediable policy error. The three files are linked pairwise through affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
