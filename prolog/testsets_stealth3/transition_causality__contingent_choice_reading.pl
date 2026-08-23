% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: The Fiat Dollar Standard as Chosen Policy Artifact (Contingent-Choice Reading of the 1971 Transition)
 *   domain: economic/monetary/international_finance
 *
 * SUMMARY:
 *   On August 15, 1971, the United States suspended gold convertibility of
 *   the dollar, ending the Bretton Woods gold-exchange standard and founding
 *   the fiat dollar order that still organizes world finance. This story
 *   authors that standing arrangement through the contingent-choice lens: the
 *   suspension was a decision among live alternatives, taken to protect
 *   domestic policy autonomy, and the arrangement built on it is a chosen
 *   policy artifact rather than a natural necessity. The ε referent is the
 *   post-1971 fiat dollar standard as this reading sees it: a system that
 *   solves a real global coordination problem — a single unit of account,
 *   settlement medium, and reserve asset for world trade — while transferring
 *   seigniorage and financing privilege to the U.S. fiscal-monetary complex
 *   and imposing cycle costs on those who hold or owe the currency.
 *   Beneficiary and victim declarations below drive the engine's
 *   directionality computation; the claim and the metrics are independent
 *   authored facts.
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: agenda setter and primary beneficiary (institutional/arbitrage) — administers the arrangement and collects seigniorage and the financing privilege
 *   - foreign_dollar_reserve_holders: primary target (powerful/trapped) — holds the reserve stock; exit devalues its own holdings
 *   - emerging_market_dollar_debtors: secondary target (powerless/trapped) — bears the cycle costs of the currency it borrows in
 *   - gold_convertibility_creditors_1971: historical victims of the founding decision (powerful/trapped) — their convertibility claims were extinguished without negotiation
 *   - us_household_borrowers: secondary beneficiary (moderate/mobile) — receives deficit-financed cheap credit
 *   - wall_street_dollar_intermediaries: secondary beneficiary (institutional/arbitrage) — runs the dollar plumbing for fees
 *   - alternative_currency_coalitions: excluded challenger (organized/constrained) — builds rails outside the governance conversation
 *   - international_monetary_economists: analytical observer (analytical/analytical) — measures the flows and maintains the documentary record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.65).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.62).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "The Fiat Dollar Standard as Chosen Policy Artifact (Contingent-Choice Reading of the 1971 Transition)").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "economic/monetary/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '1567d47a-767f-46d9-9c0a-d38ef648cb0a').
narrative_ontology:cs_kernel_codification('1567d47a-767f-46d9-9c0a-d38ef648cb0a', distributed).
narrative_ontology:cs_authority_grounding('1567d47a-767f-46d9-9c0a-d38ef648cb0a', distributed).
narrative_ontology:cs_reading_relation('1567d47a-767f-46d9-9c0a-d38ef648cb0a', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_reading_relation('1567d47a-767f-46d9-9c0a-d38ef648cb0a', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('1567d47a-767f-46d9-9c0a-d38ef648cb0a', foundational, convertibility_maintenance_was_viable).
narrative_ontology:cs_axiom_status(convertibility_maintenance_was_viable, holdable).
narrative_ontology:cs_axiom_grounding('1567d47a-767f-46d9-9c0a-d38ef648cb0a', convertibility_maintenance_was_viable, empirically_contingent).
narrative_ontology:cs_axiom('1567d47a-767f-46d9-9c0a-d38ef648cb0a', foundational, policy_choice_primary_causal_node).
narrative_ontology:cs_axiom_status(policy_choice_primary_causal_node, holdable).
narrative_ontology:cs_axiom_grounding('1567d47a-767f-46d9-9c0a-d38ef648cb0a', policy_choice_primary_causal_node, empirically_contingent).
narrative_ontology:cs_reference_frame('1567d47a-767f-46d9-9c0a-d38ef648cb0a', bretton_woods_reformable_regime).
narrative_ontology:cs_drift_state('1567d47a-767f-46d9-9c0a-d38ef648cb0a', contemporary_post_archival_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1567d47a-767f-46d9-9c0a-d38ef648cb0a', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_household_borrowers).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, wall_street_dollar_intermediaries).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, emerging_market_dollar_debtors).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, gold_convertibility_creditors_1971).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, exorbitant_privilege_doctrine).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, fiat_money_global_reserve_feasibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administer the arrangement: the Federal Reserve issues the world's reserve currency and extends swap-line backstops to selected central banks; the Treasury issues dollar debt that foreign institutions hold as the default reserve asset and operates the sanctions regime that reaches any bank clearing in dollars. They collect seigniorage on currency in circulation and finance deficits at rates unavailable to any external debtor. Exiting their own side of the arrangement would mean ceding the financing privilege they administer; they can instead shift costs onto holders unilaterally by inflating or changing policy.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__contingent_choice_reading, us_monetary_authorities, beneficiary).

% Receive cheap mortgage, credit-card, and student-loan credit backed by foreign savings that recycle into Treasury and agency debt; federal transfers and defense spending are financed in part by foreign willingness to hold dollar claims. Their exposure to the arrangement's costs (import prices, inflation episodes) is diffuse and secondary, and they can hold other currencies or assets, though few do.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_household_borrowers, beneficiary,
    moderate, biographical, mobile, national).

% Earn fees and spreads on dollar clearing, custody, foreign-exchange dealing, and the issuance and trading of dollar debt; the plumbing of the arrangement runs through their balance sheets. They are subject to the enforcement regime, since clearing jurisdiction gives regulators leverage over them, but they also profit from the volume it protects. Booking business in other currencies is possible, but the dollar book is the franchise.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, wall_street_dollar_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Central banks and sovereign funds holding the majority of global reserves in dollar assets. They accept lower yields than their scale would command in a competitive market because the depth and safety of Treasury markets are the point of holding reserves. Selling in size would devalue their own remaining holdings, so exit narrows as holdings grow; the 2022 freeze of Russian reserves demonstrated that the claims can be extinguished by political decision.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, foreign_dollar_reserve_holders, payer,
    powerful, generational, trapped, global).

% Governments and corporations that borrow in dollars because lenders will not take their own currency at scale. When the dollar strengthens or U.S. rates rise, their debt burdens balloon in local terms, forcing austerity or default; when the dollar is cheap, capital floods in and inflates bubbles. They have no seat in the governance of the currency they owe, and their collective weight is large but unorganized.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, emerging_market_dollar_debtors, payer,
    powerless, biographical, trapped, national).

% Foreign governments and central banks that held dollar claims convertible to gold at $35 per ounce under the Bretton Woods rules. On August 15, 1971, the convertibility promise was suspended without negotiation or compensation; their claims became claims on a depreciating fiat currency. The default was executed against them by decision of the debtor, and they had no recourse under the rules as rewritten.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_convertibility_creditors_1971, payer,
    powerful, biographical, trapped, global).

% Groupings building non-dollar payment and settlement rails, bilateral swap lines, and gold accumulation, and proposing basket or commodity-anchored reserve assets. They sit outside the governance conversation of the dollar system, and the enforcement regime can reach any bank that touches their rails, which raises the cost of every experiment they attempt.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, alternative_currency_coalitions, excluded,
    organized, generational, constrained, continental).

% Scholars and official-sector researchers who measure the reserve-holder yield sacrifice, the U.S. financing-cost differential, and the cycle dynamics of the arrangement; they maintain the documentary record of the 1971 decision and the estimates of the privilege's magnitude. They hold no stake in the flows and can see the whole structure; their measurement choices are themselves part of the record.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single global unit of account, settlement medium, and reserve asset: trade invoices, debt contracts, and central-bank reserves are denominated in one money, so cross-border exchange does not require per-pair clearing or a commodity settlement layer. The system supplies the safe asset that global reserve demand requires and the swap-line lender-of-last-resort capacity that dollar-debt crises require.
% TRANSFER_FUNCTION: Moves seigniorage and financing capacity from foreign dollar holders and dollar-debtors to the U.S. fiscal-monetary complex: reserve holders accept below-market yields on trillions, dollar-debtors absorb the cycle costs of U.S. monetary policy, and the difference finances U.S. deficits and consumption at prices an external debtor could not obtain.
% ABSENT_VOICES: Foreign reserve holders hold IMF and G20 seats but are outvoted under quota weights that mirror the arrangement they fund; the 1971 suspension was announced without consulting the creditors whose claims it extinguished. Builders of alternative rails are excluded from dollar-clearing governance, and the gold-standard creditors of the era left no seat in the successor design.
% DISAPPEARANCE_RATIONALE: Global trade invoicing, the multi-trillion-dollar stock of cross-border dollar credit, and every central bank's reserve portfolio are built on the arrangement. Overnight disappearance would force an unplanned re-denomination of world trade and debt, with settlement chaos until a successor reserve asset emerged. The world does not stay the same; it reorganizes at great cost.
% FOUNDING_PROBLEM: The Triffin bind as it stood in 1971: the world's growing demand for dollar reserves required the United States to run external deficits, but those deficits eroded the gold backing of the very claims being accumulated, until maintaining $35-per-ounce convertibility meant deflation at home or default abroad.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the U.S. beneficiary set: IMF External Sector Reports and the academic literature on the global safe-asset shortage (the 'new Triffin dilemma' work of Gourinchas, Rey, and Obstfeld) attest that the bind persists in mutated form; Triffin's own 1960s congressional testimony predicted it before the arrangement existed. No corroborating source attests that the suspension was the only available response: declassified options memoranda from the era list alternatives the decision set aside, and the necessity claim is asserted mainly by U.S. policymakers of the period.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.65 because the privilege is real and measured — reserve holders accept below-market yields on trillions, the United States borrows in its own currency at rates unavailable to any external debtor, and dollar-debtors absorb the cycle costs of U.S. monetary policy — but the arrangement also supplies genuine services (deep safe markets, swap-line backstops, a single unit of account), which bounds it below pure-rent levels. Suppression is 0.62 and mixed structural-enforced: network effects do much of the binding, but the enforcement layer (dollar-clearing jurisdiction, secondary sanctions, demonstrated willingness to freeze reserve claims) is load-bearing and has hardened since 2001 and again since 2022 — the suppression series tracks that hardening, which is why it is authored here. Theater is 0.35: 'strong dollar' rhetoric, surveillance communiqués, and the 'temporary' framing of the 1971 suspension coexist with real functions (swap lines, settlement plumbing), so performative activity is present but not dominant; note the non-monotone theater path (peak at the Smithsonian renegotiation, dip through the Volcker era). Accessibility_collapse is 0.5: alternatives exist (other reserve currencies, gold, new settlement rails) but each is partial, and understanding the arrangement does not dissolve it. Resistance is 0.5: de-dollarization efforts are real and accelerating (reserve diversification, alternative rails, record official gold accumulation) but have not dented primacy. I claim tangled_rope because all three structural conditions hold: a genuine coordination function, identifiable parties on both sides of the transfer, and active enforcement holding the asymmetry in place. All three series share one time grid (t = 0, 14, 27, 40, 55, 70, 82 on a 1944-origin scale), so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the agenda-setter seat the arrangement is a public good it supplies and finances: the authorities experience the privilege as the price the world pays for liquidity and stability, and their arbitrage position (they issue the liability) makes the arrangement look self-justifying. From the trapped payer seats the same structure operates as a transfer they cannot refuse: a reserve holder who sells devalues its own portfolio, a dollar-debtor who cannot borrow in its own currency has no outside option, and the 1971 creditors learned that the rules are rewritten by the debtor. The excluded challenger seat experiences the enforcement layer directly — every rail it builds can be reached through clearing jurisdiction — which the agenda-setter seat experiences as routine regulation. The engine computes per-seat classifications from these structural positions; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the authorities collect seigniorage and the financing differential, with d near the beneficiary end amplified by their arbitrage exit (they can shift costs by inflating or changing policy unilaterally); household borrowers and dollar intermediaries collect cheap credit and clearing rents with mobile and arbitrage exit respectively. Victim declarations map the other way: reserve holders bear the yield sacrifice and expropriation risk with trapped exit — large sales self-devalue, so d sits near the full-target end despite their formal power; dollar-debtors bear cycle costs with no borrowing alternative; the 1971 creditors bore the founding default outright with no recourse. Spatial scope is global for the system's core seats, which matters because verification of the arrangement's terms is hard at planetary scale and the enforcement layer exploits exactly that.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling global reserve provision with issuer solvency — is live in mutated form (the safe-asset shortage against the U.S. external position), so the arrangement is not a zombie: its coordination function is real and current. The classification blocks two opposite mislabels. It blocks the natural-law mislabel: the contingent-choice reading's entire point is that the arrangement was chosen, so emerges_naturally is false and no naturalness certification is available — the extraction is attributable to a decision, and decisions can be revisited. It blocks the pure-extraction mislabel: the coordination function (global unit of account, safe-asset supply, crisis backstops) is genuine and heavily used, so the arrangement cannot be read as coercion with a coordination cover story. What remains is the tangled middle this reading predicts: coordination that works, and a transfer that persists because the decision that founded it is still paying. If the counterfactual omega resolved toward non-viability, this reading would migrate toward its inevitable-collapse sibling and the arrangement would begin to look structurally necessitated — but that migration is the sibling's story, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_convertibility,
    'Could the United States have maintained gold convertibility through different choices — earlier devaluation, capital controls, a negotiated gold-price reform — or was the bind already unsolvable by 1971?',
    'Systematic counterfactual analysis against the documentary record: declassified options memoranda, internal Fed and Treasury estimates of gold losses under each alternative, and economic modeling of capital-flow responses to each policy path.',
    'If alternatives were viable, the arrangement''s costs are attributable to the decision and this reading''s classification stands; if not, the causal node relocates to structure, the reading collapses toward its inevitable-collapse sibling, and the arrangement''s costs read as systemic necessity rather than choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_convertibility, empirical, 'Whether maintaining convertibility was a live policy option in 1971.').

omega_variable(
    causal_node_location_omega,
    'Where does the primary causal node of the transition sit — in the August 1971 decision, in accumulated structural contradictions, or in their interaction? This story is one reading of the transition_causality kernel: the overdetermined sibling locates causation in structure alone, the hybrid sibling in structure plus trigger events.',
    'Comparative test across the readings'' predictions: the contingent reading predicts the transition''s timing and form track decision points (the Camp David meetings, the Smithsonian renegotiation, the 1973 float); the overdetermined reading predicts collapse timing insensitive to decision variation; the hybrid reading predicts trigger-dependence. Archival and econometric evidence adjudicates.',
    'Determines the arrangement''s revisability: a decision-centered causal node makes it a revisable artifact with chosen costs and chosen beneficiaries; a structure-centered node makes it a natural-seeming fixture and shifts the extraction beyond attribution to any choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_node_location_omega, conceptual, 'Kernel-level contest over the transition''s primary causal node; this story holds the contingent-choice reading.').

omega_variable(
    seigniorage_magnitude,
    'What is the actual annual value of the privilege — reserve-holder yield sacrifice, plus the U.S. financing-cost differential, plus cycle-transfer effects on dollar-debtors?',
    'BIS and IMF estimates of reserve composition and realized yields measured against a counterfactual market-rate portfolio; event studies of U.S. monetary tightening pass-through to dollar-debtor economies.',
    'Calibrates the extractiveness measure: published estimates range widely depending on method; a low estimate would pull the arrangement toward a coordination-dominant reading, a high estimate toward a transfer-dominant one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_magnitude, empirical, 'Magnitude of the exorbitant-privilege flow.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the difficulty of exiting the arrangement structural (no alternative asset at the required scale and depth) or enforced (sanctions reach, clearing jurisdiction, demonstrated expropriation of reserve claims)?',
    'Natural experiments: jurisdictions that attempted partial exit and the enforcement response they met, versus jurisdictions that drifted out passively; growth trajectories of non-dollar settlement rails under varying enforcement intensity.',
    'If exit barriers are mostly structural, the arrangement persists by default and the enforcement layer is lighter than measured; if enforced, the enforcement machinery is load-bearing and the arrangement''s persistence depends on active coercion, raising the weight of the coercive component in classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus enforced character of the exit barriers around the dollar system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 82).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tran_tr_t14, transition_causality__contingent_choice_reading, theater_ratio, 14, 0.15).
narrative_ontology:measurement(tran_tr_t27, transition_causality__contingent_choice_reading, theater_ratio, 27, 0.32).
narrative_ontology:measurement(tran_tr_t40, transition_causality__contingent_choice_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(tran_tr_t55, transition_causality__contingent_choice_reading, theater_ratio, 55, 0.3).
narrative_ontology:measurement(tran_tr_t70, transition_causality__contingent_choice_reading, theater_ratio, 70, 0.33).
narrative_ontology:measurement(tran_tr_t82, transition_causality__contingent_choice_reading, theater_ratio, 82, 0.35).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tran_be_t14, transition_causality__contingent_choice_reading, base_extractiveness, 14, 0.22).
narrative_ontology:measurement(tran_be_t27, transition_causality__contingent_choice_reading, base_extractiveness, 27, 0.45).
narrative_ontology:measurement(tran_be_t40, transition_causality__contingent_choice_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(tran_be_t55, transition_causality__contingent_choice_reading, base_extractiveness, 55, 0.62).
narrative_ontology:measurement(tran_be_t70, transition_causality__contingent_choice_reading, base_extractiveness, 70, 0.64).
narrative_ontology:measurement(tran_be_t82, transition_causality__contingent_choice_reading, base_extractiveness, 82, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__contingent_choice_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(tran_su_t14, transition_causality__contingent_choice_reading, suppression_requirement, 14, 0.15).
narrative_ontology:measurement(tran_su_t27, transition_causality__contingent_choice_reading, suppression_requirement, 27, 0.4).
narrative_ontology:measurement(tran_su_t40, transition_causality__contingent_choice_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(tran_su_t55, transition_causality__contingent_choice_reading, suppression_requirement, 55, 0.52).
narrative_ontology:measurement(tran_su_t70, transition_causality__contingent_choice_reading, suppression_requirement, 70, 0.58).
narrative_ontology:measurement(tran_su_t82, transition_causality__contingent_choice_reading, suppression_requirement, 82, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'why did Bretton Woods end?' conflates three structurally distinct claims about the same transition, per the ε-invariance principle: that the transition was a contingent policy choice (this story), that it was structurally inevitable (overdetermined_collapse_reading), and that contradictions required contingent triggers (hybrid_trigger_reading). Each reading instantiates a different constraint with its own ε, beneficiary structure, and classification: this story fixes the referent as the standing fiat-dollar arrangement seen as a chosen artifact, with extraction attributable to the 1971 decision; an inevitability reading would fix the same referent as a structurally necessitated order, with the costs beyond attribution. The three stories form one kernel family; this story links to both siblings and holds the decision-centered axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
