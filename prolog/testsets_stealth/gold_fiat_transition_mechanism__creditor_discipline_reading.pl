% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Post-Bretton Woods Asymmetric Adjustment Regime (Creditor-Discipline Reading)
 *   domain: economic/political/monetary-history
 *
 * SUMMARY:
 *   This story instantiates the creditor-discipline reading of the gold-fiat
 *   transition kernel: the operative change of 1971 was the destruction of
 *   the creditor's veto — the right to demand gold for dollar claims — which
 *   converted a balance-of-payments discipline that had bound the reserve
 *   issuer into a discipline that binds only everyone else. The standing
 *   arrangement under classification is the post-transition regime: the
 *   issuer settles its deficits in its own unredeemable paper while
 *   non-reserve debtors face rate-cycle shocks, freezable reserves, and
 *   lender-imposed adjustment. The claim/metric split is deliberate: the
 *   arrangement is CLAIMED as tangled_rope — genuine global liquidity
 *   coordination fused with asymmetric extraction — while the metrics
 *   describe strongly extractive, increasingly enforced operation; the engine
 *   measures that divergence rather than the author reconciling it. Sibling
 *   readings (automatic-constraint, composite-overdetermination) are separate
 *   constraints with separate files; see the omegas and kernel_context for
 *   the committer structure.
 *
 * KEY AGENTS:
 *   - - reserve_currency_issuer: Primary beneficiary and agenda-setter (institutional/arbitrage) — issues the settlement asset, exempt from redemption, sets system-wide credit prices
 *   - - creditor_nations: Primary victim of this reading (powerful/constrained) — lost the redemption lever; holdings now irredeemable paper
 *   - - foreign_dollar_reserve_holders: Secondary victim (organized/trapped) — carry inflation tax and freeze risk on mandatory buffers
 *   - - non_reserve_debtor_nations: Secondary victim (powerless/trapped) — discipline tightened for them as it vanished for the issuer
 *   - - dollar_swap_line_allies: Secondary beneficiary (institutional/constrained) — crisis liquidity as a perk of system membership
 *   - - international_monetary_fund: Enforcement administrator, dual-positioned (institutional/identity_locked) — administers periphery adjustment, expands with each program
 *   - - gold_accumulating_central_banks: Excluded challenger (organized/constrained) — diversifying out, no seat in governance
 *   - - monetary_historians: Analytical observer (analytical/analytical) — documents the transition and structures the reading contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.8).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.72).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Post-Bretton Woods Asymmetric Adjustment Regime (Creditor-Discipline Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "economic/political/monetary-history").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '081d0459-d4eb-49c0-a85e-f59fea537afd').
narrative_ontology:cs_kernel_codification('081d0459-d4eb-49c0-a85e-f59fea537afd', distributed).
narrative_ontology:cs_authority_grounding('081d0459-d4eb-49c0-a85e-f59fea537afd', distributed).
narrative_ontology:cs_reading_relation('081d0459-d4eb-49c0-a85e-f59fea537afd', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('081d0459-d4eb-49c0-a85e-f59fea537afd', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('081d0459-d4eb-49c0-a85e-f59fea537afd', foundational, redemption_leverage_was_operative_discipline).
narrative_ontology:cs_axiom_status(redemption_leverage_was_operative_discipline, holdable).
narrative_ontology:cs_axiom_grounding('081d0459-d4eb-49c0-a85e-f59fea537afd', redemption_leverage_was_operative_discipline, empirically_contingent).
narrative_ontology:cs_axiom('081d0459-d4eb-49c0-a85e-f59fea537afd', secondary, issuer_autonomy_is_structural_power).
narrative_ontology:cs_axiom_status(issuer_autonomy_is_structural_power, holdable).
narrative_ontology:cs_axiom_grounding('081d0459-d4eb-49c0-a85e-f59fea537afd', issuer_autonomy_is_structural_power, empirically_contingent).
narrative_ontology:cs_reference_frame('081d0459-d4eb-49c0-a85e-f59fea537afd', creditor_veto_discipline_regime).
narrative_ontology:cs_drift_state('081d0459-d4eb-49c0-a85e-f59fea537afd', contemporary_post_transition_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('081d0459-d4eb-49c0-a85e-f59fea537afd', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, dollar_swap_line_allies).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, international_monetary_fund).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, hegemonic_stability_theory).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__creditor_discipline_reading, exorbitant_privilege_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the currency the rest of the world uses to settle trade and hold savings. Runs persistent external deficits financed by its own IOUs, which foreign holders must accept because no redemption window exists. Sets the price of dollar credit for the entire system through its central bank and polices access to dollar clearing through sanctions law. Settling up, for this actor, means printing the settlement asset itself.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).

% Export more than they import and accumulate the issuer's paper as the byproduct. Before 1971 they could present that paper for gold and force the issuer to tighten; since the window closed, the paper converts only into more paper. Their leverage has shrunk to persuasion, gradual reserve diversification, and the threat — rarely usable — of selling holdings whose value depends on not selling them.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, generational, constrained, global).

% Central banks of smaller and developing economies hold large dollar balances as self-insurance against the sudden stops and currency runs that recur in the system. The balances earn little, lose purchasing power when the issuer inflates, and can be frozen by the issuer's courts and treasury. Exiting means inviting the very crisis the balances insure against.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, foreign_dollar_reserve_holders, payer,
    organized, biographical, trapped, global).

% Borrow in the issuer's currency because their own is not accepted abroad. When the issuer tightens, their debts swell in local terms and capital flees; they then submit to lender-imposed programs of austerity and privatization to regain market access. Their fiscal room narrowed, not widened, when the old redemption regime ended.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations, payer,
    powerless, biographical, trapped, regional).

% A handful of allied central banks hold standing arrangements to exchange their currencies for dollars in a crisis. Their large banks fund themselves in dollars daily; the swap line is the difference between a liquidity scare and a solvency event. Membership comes with integration so deep that leaving would break their own banking systems.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, dollar_swap_line_allies, beneficiary,
    institutional, generational, constrained, continental).

% Lends to countries shut out of private markets, attaching conditions on spending, subsidies, and ownership. Administers the adjustment that deficit countries must undergo — adjustment the issuer of the reserve asset never undergoes. Its budget, staffing, and mandate expand with each lending program, and its governance weights reflect the issuer and its allies. The institution has grown into its crisis-lending role to the point where its identity and that role are inseparable.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_monetary_fund, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, international_monetary_fund, beneficiary).

% Central banks, chiefly in Asia and Eurasia, adding gold to reserves and building bilateral settlement channels since reserve freezes demonstrated that dollar balances are conditional. They would argue for a multipolar settlement system; they hold no vote in the forums where the current one is governed, and their diversification proceeds slowly because their trade remains dollar-invoiced.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_accumulating_central_banks, excluded,
    organized, generational, constrained, continental).

% Economists and historians who reconstruct what the 1971 rupture did and did not change. They work from archives — treasury memos, central bank minutes, negotiation records — and publish competing accounts of the transition's causes. Their disagreements structure the debate this story sits inside.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, monetary_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies the world economy with a single settlement asset, unit of account, and deep market in safe claims, replacing a gold stock that could not grow fast enough to support expanding trade — the liquidity shortage contemporaries knew as the Triffin dilemma.
% TRANSFER_FUNCTION: Moves real goods, services, and policy discretion from the rest of the world to the reserve issuer: exporters accept paper claims that are never redeemed, savers absorb the issuer's inflation, and deficit countries outside the issuer's chair undergo adjustment programs the issuer itself is exempt from. After 1971 the direction of compulsory adjustment reversed — the creditor's veto over the debtor became the periphery's subordination to the issuer's interest-rate cycle.
% ABSENT_VOICES: The surplus countries that once exercised the veto left the argument when the window closed — their objection was answered by abolition, not rebuttal. Developing-economy debtors hold fund voting shares far below their economic weight, and the gold-diversifying central banks have no seat in the Basel-Washington forums that govern dollar clearing. Each would argue for symmetric adjustment obligations.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the pricing of world trade, force a scramble for substitute settlement assets, trigger a financing crisis at the issuer, and fire-sale the reserve portfolios of every central bank. The trading system would reorganize around whatever replaced the dollar's clearing role, at enormous transitional cost.
% FOUNDING_PROBLEM: In its post-1971 form, the arrangement was built to solve the collapse of the gold-exchange standard: world trade had outgrown the gold stock, and the issuer could no longer honor redemption without crushing domestic expansion. The founding choice was to keep the dollar's reserve role while severing the redemption obligation that had made creditors partners in discipline.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the People's Bank of China's 2009 call for a supranational reserve asset concedes the liquidity function is real while disputing its stewardship; BIS working papers and the academic literature descending from Triffin (1960) document the reserve-shortage problem independently of any issuer interest. No party inside the system disputes that the liquidity problem exists; the dispute is over who pays for solving it this way.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.80 at interval end) because the regime's transfers are decoupled from any service the recipients could refuse: seigniorage on world settlement, an inflation tax on mandatory reserve holdings, sanction rents on clearing access, and adjustment burdens applied asymmetrically. Suppression (0.72) is a raw structural property, unscaled by power or scope: it combines network lock-in (invoice and clearing inertia) with coercive enforcement (sanctions law, secondary sanctions, reserve freezes), and its rising series models the enforcement machinery maturing from the informal petrodollar arrangements of the 1970s to the institutionalized freeze-and-exclude toolkit after 2012 and 2022. Theater ratio (0.42) captures the widening gap between multilateral discipline rhetoric — fund surveillance that ritually criticizes issuer imbalances with zero consequence, rebalancing pledges quietly abandoned, 'strong dollar' boilerplate — and the bilateral, geopolitical reality of enforcement. Accessibility collapse (0.55) is moderate: partial exits exist (euro and yuan settlement, gold, bilateral swap networks) but none reaches the invoicing-and-clearing core. Resistance (0.60) is real and growing: dedollarization discourse, sustained official-sector gold accumulation, regional liquidity arrangements. The measurement series run on one shared seven-point grid (time_point n = year 1971+n) so every tracked metric is authored at every examined time point; the trajectories are monotonic by design — this is an enforcement ratchet, not an oscillating cycle. Fixing is prohibitive for every seat that could attempt it: the issuer would have to renounce its financing and coercive advantages, and any holder coalition large enough to matter would destroy the value of its own holdings in the act.
 *
 * PERSPECTIVAL GAP:
 *   From the issuer's chair the regime is the price of a liquidity service the world demands voluntarily; from the creditor seats it is confiscation of claims that were once redeemable; from the periphery's seats it is a discipline that tightened at the precise moment the strongest debtor escaped its own. One structure, three computed experiences — the engine derives them from the power, exit, and role data rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The issuer declares as beneficiary with arbitrage-grade exit (it settles in what it prints), placing it at the full-beneficiary end; the swap-line allies declare as beneficiaries with constrained exit, sitting low but not zero. The three victim groups drive the target end: creditors (constrained — their holdings' value depends on continued participation), reserve holders (trapped — buffers are self-insurance against crises the system generates), and non-reserve debtors (trapped — they borrow in the money they cannot issue). The fund is dual-positioned: it administers the regime and expands with it, but its identity fusion with the crisis-lending role complicates any simple beneficiary reading. The vindicated propositions (hegemonic-stability theory, the exorbitant-privilege thesis) collect no rents and are deliberately kept out of the beneficiary set.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards in both directions. Reading the regime as pure coordination erases the confiscation this reading identifies as the transition's operative content; reading it as pure extraction erases the liquidity service every crisis re-confirms — the founding problem is live, corroborated from outside the benefiting parties. Tangled rope holds both halves. The mandatrophy trap sits elsewhere: the founding problem of the OLD regime (creditor-enforced discipline of the issuer) is dead, and a lazy lifecycle read could mark the whole arrangement vestigial. It is not — the arrangement's extraction is current, not residual, and no seat profits so diffusely that maintenance is mere performance; the dead part (the veto) and the live part (elastic liquidity) are exactly what the live founding-problem status plus the named victim structure encode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the transition''s operative change the elimination of creditor veto power (this reading), the replacement of a material anchor with discretionary authority (automatic_constraint_reading), or the convergence of multiple independent structural changes (composite_overdetermination_reading)?',
    'Counterfactual historiography: if telecommunications and legal-tender maturation alone predict post-1971 debtor flexibility, the composite reading absorbs this one; if redemption-threat removal alone predicts the issuer''s fiscal trajectory, this reading stands; if the binding change was the anchor''s substance regardless of who held leverage, the automatic reading stands.',
    'Determines which constraint the corpus is classifying: this file''s high-epsilon discipline-asymmetry structure, or a different structure with different beneficiaries and a different epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which of the three readings of the gold-fiat transition kernel identifies the operative causal change.').

omega_variable(
    beneficiary_concentration,
    'Does the fiscal-flexibility gain from the veto''s elimination extend to debtor nations generally, or concentrate wholly in the reserve-currency issuer?',
    'Compare pre- and post-1971 borrowing terms, adjustment frequency, and policy autonomy for the reserve issuer versus peripheral debtors; the expected structural delta says ''especially US'', which if literal narrows the beneficiary set to the issuer seat.',
    'If concentrated, the beneficiary declaration collapses toward the issuer alone and effective extraction on the periphery rises further; if general, the regime carries a broader coordination dividend and the tangled-rope reading strengthens against snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration, empirical, 'Whether the transition''s debtor-side benefit was systemic or issuer-specific.').

omega_variable(
    holder_lock_in_mechanism,
    'Is non-reserve holder lock-in structural (network externalities, invoicing and clearing inertia) or coercive (sanctions, market-access, and freeze threats)?',
    'Natural experiments across jurisdictions: compare exit costs for states facing explicit coercion versus states merely embedded in the network, net of threat effects; track post-freeze diversification rates against pre-freeze baselines.',
    'If coercive dominance grows, measured suppression understates the regime''s hold and the structure trends toward snare; if network effects dominate, the suppression reflects coordination cost inherent to any settlement standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holder_lock_in_mechanism, empirical, 'Structural versus coercive composition of the regime''s hold on reserve holders.').

omega_variable(
    periphery_discipline_decomposition,
    'Is the tightened discipline borne by non-reserve debtors part of THIS constraint (the asymmetric adjustment regime) or a separate constraint (the conditionality and sudden-stop machinery) deserving its own story?',
    'Apply the epsilon-invariance test: if measuring the regime with and without the conditionality apparatus yields materially different epsilon values, decompose into two linked stories — this one keeping the issuer-creditor asymmetry, a sibling carrying the periphery-conditionality structure.',
    'Decomposition would split the constraint family: the sibling would name the fund and private creditors as its agenda-setters, changing the victim set and the computed per-seat classifications for every shared agent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periphery_discipline_decomposition, conceptual, 'Whether periphery-side tightening belongs to this story or to a separate linked constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(gold_tr_t0, observed).
narrative_ontology:measurement(gold_tr_t9, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 9, 0.24).
narrative_ontology:measurement_basis(gold_tr_t9, observed).
narrative_ontology:measurement(gold_tr_t18, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement_basis(gold_tr_t18, observed).
narrative_ontology:measurement(gold_tr_t27, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 27, 0.32).
narrative_ontology:measurement_basis(gold_tr_t27, observed).
narrative_ontology:measurement(gold_tr_t36, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 36, 0.34).
narrative_ontology:measurement_basis(gold_tr_t36, observed).
narrative_ontology:measurement(gold_tr_t45, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement_basis(gold_tr_t45, observed).
narrative_ontology:measurement(gold_tr_t54, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 54, 0.42).
narrative_ontology:measurement_basis(gold_tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(gold_be_t0, observed).
narrative_ontology:measurement(gold_be_t9, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 9, 0.66).
narrative_ontology:measurement_basis(gold_be_t9, observed).
narrative_ontology:measurement(gold_be_t18, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement_basis(gold_be_t18, observed).
narrative_ontology:measurement(gold_be_t27, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 27, 0.74).
narrative_ontology:measurement_basis(gold_be_t27, observed).
narrative_ontology:measurement(gold_be_t36, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 36, 0.76).
narrative_ontology:measurement_basis(gold_be_t36, observed).
narrative_ontology:measurement(gold_be_t45, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 45, 0.78).
narrative_ontology:measurement_basis(gold_be_t45, observed).
narrative_ontology:measurement(gold_be_t54, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 54, 0.8).
narrative_ontology:measurement_basis(gold_be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(gold_su_t0, observed).
narrative_ontology:measurement(gold_su_t9, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 9, 0.45).
narrative_ontology:measurement_basis(gold_su_t9, observed).
narrative_ontology:measurement(gold_su_t18, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement_basis(gold_su_t18, observed).
narrative_ontology:measurement(gold_su_t27, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 27, 0.58).
narrative_ontology:measurement_basis(gold_su_t27, observed).
narrative_ontology:measurement(gold_su_t36, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement_basis(gold_su_t36, observed).
narrative_ontology:measurement(gold_su_t45, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement_basis(gold_su_t45, observed).
narrative_ontology:measurement(gold_su_t54, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 54, 0.72).
narrative_ontology:measurement_basis(gold_su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, petrodollar_recycling_arrangement).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__creditor_discipline_reading, imf_conditionality_lending).

% DUAL FORMULATION NOTE:
% Constraint family: gold_fiat_transition_mechanism decomposes into three readings per the epsilon-invariance principle. The automatic-constraint reading's referent (anchor substance) admits near-zero extraction; the composite reading distributes causality across independent changes and resists a single stable epsilon; this reading isolates the discipline-incidence change, whose referent — the post-1971 asymmetric adjustment regime — carries high epsilon with the issuer as beneficiary and creditors as victims. The upstream historical fact of the transition is cited as evidence within this reading; this reading in turn grounds the downstream petrodollar-recycling and IMF-conditionality stories. Every family member links to at least one other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
