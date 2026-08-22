% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__composite_overdetermination_reading, []).

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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Composite Overdetermination Reading of the Gold-Fiat Transition
 *   domain: monetary economics/political economy/history of economic thought
 *
 * SUMMARY:
 *   This story instantiates the composite_overdetermination_reading of the
 *   gold_fiat_transition_mechanism kernel. On this reading, the move from
 *   gold-anchored to fiat money between the late 1950s and circa 1990 was not
 *   a single swap executed at one node: it was the convergence of four
 *   independent structural changes — telecommunications capacity making
 *   same-day cross-border settlement physically possible, the collapse of the
 *   Bretton Woods peg system, the erosion of organized labor's bargaining
 *   power (which replaced gold discipline with wage discipline in the
 *   anti-inflation architecture), and the maturation of legal tender
 *   enforcement (which made state money universally acceptable by
 *   administrative fiat rather than redeemability). The Nixon Shock of August
 *   1971 was a symbolic marker of processes already underway, not their
 *   cause. The epsilon referent is the standing post-transition fiat monetary
 *   arrangement assessed by this reading's own lights: a structure that
 *   coordinates planetary exchange while distributing its costs unevenly
 *   across the losers of each pillar — savers bearing the inflation tax,
 *   labor bearing the wage-discipline burden, peripheral economies bearing
 *   capital-flow volatility. Because different pillars had different
 *   distributional effects, no single seat captures the aggregate; the
 *   reading's moderate epsilon reflects genuine coordination carrying
 *   non-trivial, heterogeneously incident extraction. The claim/metric gap
 *   discipline applies: claimed_type is authored from structural analysis of
 *   the composite arrangement; the metrics are authored descriptively from
 *   the historical record; the engine computes per-seat classifications
 *   independently.
 *
 * KEY AGENTS:
 *   - - reserve_currency_central_banks: Agenda setter (institutional/identity_locked) — administers fiat issuance and settlement, collects seigniorage, cannot exit without dissolving its own purpose
 *   - - transnational_financial_institutions: Primary per-pillar beneficiary (powerful/arbitrage) — profits from telecom-enabled capital mobility, relocates faster than regulation
 *   - - global_telecom_network_operators: Infrastructure beneficiary (institutional/mobile) — owns the load-bearing cables and switches, sets no monetary rules
 *   - - indebted_sovereign_governments: Dual-positioned (institutional/constrained) — gained fiscal flexibility from the peg collapse, exposed to sudden-stop reversals
 *   - - organized_labor_unions: Payer (organized/constrained) — bore the wage-discipline burden as bargaining power eroded across the interval
 *   - - cash_savers_fixed_income_holders: Payer (moderate/constrained) — bears the inflation tax; partial hedges exist, full exit blocked by tax denomination
 *   - - peripheral_economy_governments: Payer (moderate/trapped) — formally sovereign, financially dependent, exposed to volatile flows they did not choose
 *   - - hard_money_advocates: Excluded voice (moderate/trapped) — lost the policy conversation after 1971; the 1980s US gold commission heard them and changed nothing
 *   - - monetary_history_scholars: Analytical observer (analytical/analytical) — produce the competing kernel readings; neither collect nor pay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.46).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.39).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.29).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.39).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.29).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Composite Overdetermination Reading of the Gold-Fiat Transition").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "monetary economics/political economy/history of economic thought").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, '0d339a38-21d6-4877-9919-4797b60df419').
narrative_ontology:cs_kernel_codification('0d339a38-21d6-4877-9919-4797b60df419', distributed).
narrative_ontology:cs_authority_grounding('0d339a38-21d6-4877-9919-4797b60df419', distributed).
narrative_ontology:cs_reading_relation('0d339a38-21d6-4877-9919-4797b60df419', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('0d339a38-21d6-4877-9919-4797b60df419', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('0d339a38-21d6-4877-9919-4797b60df419', foundational, no_single_transition_node).
narrative_ontology:cs_axiom_status(no_single_transition_node, holdable).
narrative_ontology:cs_axiom_grounding('0d339a38-21d6-4877-9919-4797b60df419', no_single_transition_node, empirically_contingent).
narrative_ontology:cs_axiom('0d339a38-21d6-4877-9919-4797b60df419', foundational, nixon_shock_symbolic_marker_only).
narrative_ontology:cs_axiom_status(nixon_shock_symbolic_marker_only, holdable).
narrative_ontology:cs_axiom_grounding('0d339a38-21d6-4877-9919-4797b60df419', nixon_shock_symbolic_marker_only, empirically_contingent).
narrative_ontology:cs_axiom('0d339a38-21d6-4877-9919-4797b60df419', secondary, pillar_specific_distributional_effects).
narrative_ontology:cs_axiom_status(pillar_specific_distributional_effects, holdable).
narrative_ontology:cs_axiom_grounding('0d339a38-21d6-4877-9919-4797b60df419', pillar_specific_distributional_effects, empirically_contingent).
narrative_ontology:cs_reference_frame('0d339a38-21d6-4877-9919-4797b60df419', overdetermined_multi_pillar_convergence).
narrative_ontology:cs_drift_state('0d339a38-21d6-4877-9919-4797b60df419', contemporary_monetary_historiography, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('0d339a38-21d6-4877-9919-4797b60df419', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_central_banks).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, transnational_financial_institutions).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_telecom_network_operators).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, indebted_sovereign_governments).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, cash_savers_fixed_income_holders).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized_labor_unions).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economy_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, indebted_sovereign_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate fiat issuance, set policy rates, and act as lenders of last resort within the post-gold settlement system. Their staffing, mandates, and public legitimacy are built around managing discretionary money; abandoning the arrangement would dissolve the institutions themselves. They collect seigniorage and expand balance sheets without facing external redemption demands.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_central_banks, agenda_setter,
    institutional, generational, identity_locked, global).

% Profit from the instantaneous cross-border capital movement the telecommunications layer made possible: foreign-exchange dealing, carry trades, offshore booking. They can relocate booking entities across jurisdictions faster than regulation adapts, and that mobility is a direct product of the arrangement's first pillar.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, transnational_financial_institutions, beneficiary,
    powerful, biographical, arbitrage, global).

% Own the submarine cables, satellite links, and switching infrastructure that made same-day settlement physically feasible; payment messaging volume became a durable revenue base once capital flows went digital. Their networks are load-bearing for the arrangement, but they neither set nor enforce monetary rules.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_telecom_network_operators, beneficiary,
    institutional, generational, mobile, global).

% Gained fiscal flexibility when redemption threats ended and exchange rates floated: deficits no longer trigger reserve drains. The same openness exposes them to sudden-stop capital flight and lender conditionality when flows reverse; the flexibility and the vulnerability arrived together.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, indebted_sovereign_governments, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, indebted_sovereign_governments, payer).

% Saw bargaining power erode across the interval as wage restraint replaced gold discipline in the anti-inflation architecture; wage moderation became a policy expectation backed by unemployment costs. Members cannot exit the wage relation, and geographic and sectoral mobility are limited.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized_labor_unions, payer,
    organized, biographical, constrained, national).

% Hold money balances whose purchasing power the issuer dilutes; the 1970s inflation took double-digit annual bites. Partial hedges exist in equities, property, and foreign currency, but full exit is blocked because taxes are payable only in state money and wages arrive in it.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, cash_savers_fixed_income_holders, payer,
    moderate, biographical, constrained, national).

% Formally sovereign but financially dependent: their currencies are not accepted abroad, so they must earn or borrow reserve currency, exposing them to volatile inflows and disorderly reversals they did not choose. Standard-setting bodies weight them lightly in governance.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, peripheral_economy_governments, payer,
    moderate, generational, trapped, regional).

% Economists, legislators, and activists arguing for commodity anchoring or strict issue rules. After 1971 they lost the policy conversation: the early-1980s United States gold commission received their testimony and recommended no change. Their exclusion is maintained by the enforcement pillar's practical success rather than by settled argument.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, hard_money_advocates, excluded,
    moderate, generational, trapped, national).

% Reconstruct the transition from archives, price series, and institutional records, producing the competing causal readings this kernel carries. They neither collect nor pay; their disputes determine which account disciplines future interpretation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_history_scholars, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform unit of account and settlement medium for planetary-scale exchange; floating rates absorb asymmetric shocks that fixed pegs transmitted; instant payment rails enable just-in-time trade finance; matured legal tender enforcement guarantees universal acceptability of the settlement medium without redeemability. Each pillar solves a distinct coordination problem that gold convertibility increasingly failed to solve as trade outgrew gold liquidity (the Triffin dilemma).
% TRANSFER_FUNCTION: Moves purchasing power from money holders to issuers via seigniorage and the inflation tax; moves bargaining surplus from organized labor to employers as wage discipline replaced gold discipline; moves flow risk onto financially dependent economies through volatile capital movements; moves transaction-cost savings to traders and financial intermediaries through instant settlement.
% ABSENT_VOICES: Hard-money advocates were shut out of the post-1971 policy conversation — the gold commission episode shows participation without effect. Populations of peripheral economies bear capital-flow volatility with minimal representation in the bodies that set financial standards, given quota-weighted governance. Savers hold no seat in rate-setting deliberations. These absences matter for consensus provenance: the apparent settlement around fiat management reflects who was in the room, not unanimity among all affected parties.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, every price, wage, contract, and sovereign debt instrument denominated in state money would require immediate renegotiation; trade finance would seize; the payments infrastructure built on the telecom pillar would lose its settlement object. The world does not merely continue minus this constraint — it reorganizes around whatever replaces the unit of account.
% FOUNDING_PROBLEM: Gold convertibility could not supply sufficient international liquidity for growing world trade (the Triffin dilemma), and fixed pegs transmitted United States deficits outward while constraining domestic policy; separately, uniform currency acceptance required enforcement machinery where redeemability-based trust was unavailable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: economic historians across schools — including sympathizers of the gold standard such as Eichengreen — document the liquidity shortfall and peg strains from the archival record; contemporaneous testimony by policymakers who administered the old regime (including Federal Reserve officials of the late 1960s) attests the problem was live before the transition; hard-money critics acknowledge the liquidity problem while disputing the chosen solution. No party inside the beneficiary set is the sole attestor.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).
:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end): the arrangement supplies the largest coordination good in economic history — a uniform unit of account and settlement medium — while diluting holder purchasing power, taxing cash balances through inflation, and imposing flow volatility on financially dependent states. Suppression (0.39) is structural rather than interpersonal: legal tender statutes, tax denomination in state money, and the historical prohibition of private gold holdings close exits, but foreign currency, hard assets, and equities remain partially accessible, so alternatives are narrowed rather than eliminated. Theater (0.29) is modest and rising slowly: the operational machinery is real, but mandate performance, communication ritual, and institutional self-presentation grew as central banking professionalized. Accessibility_collapse (0.45) and resistance (0.38) sit mid-range accordingly: hard-money movements, the 1980s gold commission, and partial re-anchoring attempts (the European Monetary System) met the arrangement without dislodging it. The temporal series run on one shared grid (nine points, all three metrics at every point) as the alignment rule requires. The base_extractiveness series is cyclical rather than monotonic: it climbs through the 1970s inflation surge (peaking near t=20, the late-1970s double-digit inflation), then falls through the Volcker disinflation before settling slightly above its pre-surge level. The cycle is a side effect of policy error and external supply shocks followed by corrective learning — not an engineered intermittent-reinforcement mechanism — and the end-state scalars are measured at the settled phase (t=32), not at the 1970s peak. The suppression_requirement series is authored deliberately as a falling trajectory because the story's enforcement history is one of capacity decay, not ratcheting: United States gold ownership was prohibited until 1974, capital controls were pervasive through the 1960s and liberalized through the late 1970s and 1980s, and by interval end the arrangement ran substantially on habituation and network effects rather than active coercion. A flat scalar would misrepresent that decay; the residual uptick at t=32 reflects renewed regulatory attention to monetary alternatives as electronic transfer matured.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the reserve-currency central bank's position the arrangement is the coordination system it operates and staffs — its identity is fused with discretionary money management, so exit is unthinkable without institutional dissolution (identity_locked). From the cash saver's position the same arrangement is a slow levy on stored purchasing power with hedged but never fully closed exits. Transnational financial institutions experience the arrangement as opportunity space: their arbitrage-grade exit options place them nearest the beneficiary pole of any seat. Peripheral governments and organized labor occupy the target side despite nominal power differences — a same-level contrast: two sets of sovereign governments (core reserve issuers versus peripheral borrowers) hold identical formal standing but radically different exit options, because only one side's currency is accepted abroad. The engine derives this divergence from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations map to per-pillar structural relationships rather than a single axis. Reserve-currency central banks sit near the beneficiary end as administrators collecting seigniorage; transnational financial institutions sit nearest the beneficiary pole of all seats because their exit options are arbitrage-grade — the constraint subsidizes precisely their mobility. Global telecom operators benefit incidentally as infrastructure owners without administering rules. Indebted sovereign governments are genuinely dual-positioned: the peg collapse subsidized their fiscal flexibility (low d) while flow volatility taxes them (raising d), netting to a mid-low value. Cash savers sit near the target end: they bear the inflation tax and their exit is constrained by tax denomination. Organized labor bears the wage-discipline pillar's costs with constrained exit. Peripheral governments are trapped targets — highest d among the payers — because their currencies are not accepted abroad and standard-setting bodies weight them lightly. The composite of these positions yields the moderate aggregate epsilon the reading predicts; no seat sits at an extreme except the financial-institution beneficiary pole.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading localizes obsolescence at the component level, not the arrangement level, and the classification apparatus respects that granularity. The founding problem — supplying elastic international liquidity for growing trade where gold scarcity could not (the Triffin dilemma), plus absorbing asymmetric shocks that fixed pegs transmitted — remains live, so the arrangement is not a zombie: status=live combined with verdict=world_rearranges raises no capture flag. Yet individual components show mandate drift: the IMF's original peg-surveillance mandate outlived the pegs it surveilled, and portions of the legal-tender enforcement apparatus matured past necessity into habituation. Claiming tangled_rope rather than rope prevents the coordination-only celebration that would erase the per-pillar losers; refusing snare prevents the conspiratorial single-extractor reading that the evidence contradicts — the extraction is real but heterogeneous, incident on different seats through different pillars, with no unified capturer. Mandatrophy resolution therefore tracks the reading's own thesis: the transition had no single mandate to atrophy, so atrophy must be audited pillar-by-pillar.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the gold_fiat_transition_mechanism kernel: did the transition reduce to a single mechanism (automatic constraint removal, or creditor-veto elimination), or was it overdetermined convergence of independent changes?',
    'Comparative causal analysis of the three readings against the archival and econometric record: if every strand (telecom capacity, peg collapse, labor shift, legal tender maturation) traces to one causal node, the composite reading collapses into whichever sibling owns that node.',
    'If a sibling reading is correct, this constraint''s beneficiary/victim structure is mis-specified: the unified mechanism''s winners and losers replace the per-pillar distribution, and the aggregate epsilon attaches to a different arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the transition kernel holds; this file instantiates the composite reading only.').

omega_variable(
    pillar_common_cause,
    'Are the four structural changes genuinely independent, or do they share a common driver (United States fiscal expansion, Cold War spending, a single policy regime) that would restore single-cause structure?',
    'Timing and coupling analysis: test whether each pillar''s onset is institutionally and statistically coupled to the others or separable in the record.',
    'A demonstrated common cause would reclassify the transition as single-node, concentrating directionality on that node''s winners and losers and undermining the reading''s signature claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pillar_common_cause, empirical, 'Independence of the four convergent structural changes.').

omega_variable(
    pillar_necessity_counterfactual,
    'Was each pillar necessary for the fiat order''s consolidation, or are they redundant substitutes such that any subset would have sufficed?',
    'Natural experiments: jurisdictions or periods lacking one pillar (capital controls blocking telecom-enabled flows; economies retaining formal wage indexation) — did fiat consolidation stall or reverse there?',
    'Redundancy would lower the arrangement''s fragility reading and weaken the overdetermination claim''s explanatory force; strict necessity would strengthen it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pillar_necessity_counterfactual, empirical, 'Per-pillar necessity versus substitutability in the transition.').

omega_variable(
    aggregate_epsilon_well_definedness,
    'With distributional effects differing pillar-by-pillar, is a single scalar epsilon for the composite arrangement well-defined, or does the structure resist aggregation?',
    'Decomposition analysis: compute per-pillar extraction and test whether weighted aggregation is stable across reasonable weighting schemes.',
    'If unstable, the engine''s scalar effective-extraction value for this reading carries wide uncertainty and seat-level computation should dominate the aggregate in any verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_epsilon_well_definedness, conceptual, 'Whether heterogeneous per-pillar incidence aggregates into a stable scalar.').

omega_variable(
    nixon_shock_causal_weight,
    'Did the August 1971 suspension carry independent causal weight (expectational break, precedent-setting) beyond marking changes already underway?',
    'Event-study analysis of expectations and policy behavior around August 1971 versus trend extrapolation from 1968-1970 data.',
    'If the Shock was causal rather than symbolic, the composite reading''s signature claim weakens and the timeline of benefit accrual shifts earlier than the convergence account allows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nixon_shock_causal_weight, empirical, 'Causal versus symbolic status of the Nixon Shock within the convergence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gold_tr_t4, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(gold_tr_t8, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(gold_tr_t12, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(gold_tr_t16, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(gold_tr_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement(gold_tr_t24, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(gold_tr_t28, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 28, 0.27).
narrative_ontology:measurement(gold_tr_t32, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 32, 0.29).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gold_be_t4, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 4, 0.25).
narrative_ontology:measurement(gold_be_t8, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(gold_be_t12, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(gold_be_t16, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(gold_be_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gold_be_t24, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(gold_be_t28, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 28, 0.44).
narrative_ontology:measurement(gold_be_t32, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 32, 0.46).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gold_su_t4, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement(gold_su_t8, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(gold_su_t12, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 12, 0.5).
narrative_ontology:measurement(gold_su_t16, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(gold_su_t20, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(gold_su_t24, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(gold_su_t28, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 28, 0.38).
narrative_ontology:measurement(gold_su_t32, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 32, 0.39).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel gold_fiat_transition_mechanism. The colloquial label 'the gold-to-fiat transition' covers three structurally distinct causal claims with different epsilon values and different beneficiary/victim structures: the automatic reading's epsilon attaches to the discretionary-central-bank arrangement; the creditor reading's epsilon attaches to the debtor-flexibility arrangement; this reading's epsilon attaches to the composite arrangement with per-pillar incidence and no unified capturer. The composite reading is upstream of both siblings: it does not deny that gold-reserve limits loosened or that creditor veto eroded — it denies either was THE transition — so it exerts structural pressure on the siblings to defend single-cause attribution without logically eliminating them. Each sibling file should carry a reciprocal affects_constraints edge back to this story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
