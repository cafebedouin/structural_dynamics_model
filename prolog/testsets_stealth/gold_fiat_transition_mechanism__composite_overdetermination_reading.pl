% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__composite_overdetermination_reading
 *   human_readable: Post-Transition Fiat Monetary Arrangement as Emergent Convergence (Composite Overdetermination Reading)
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'gold_fiat_transition_mechanism': the composite-overdetermination
 *   reading, under which the standing post-transition arrangement - the
 *   unanchored fiat monetary system that consolidated between the late 1960s
 *   and 1990 - is not a designed replacement of one constraint with another
 *   but the residue of four independent structural changes that converged:
 *   telecommunications enabling instant cross-border capital flows, the
 *   collapse of the Bretton Woods pegs, the shift in labor bargaining power,
 *   and the maturation of legal-tender enforcement. On this reading the
 *   August 1971 announcement was a symbolic marker of a consolidation already
 *   underway, not its causal node. The constraint under classification is
 *   therefore the standing fiat arrangement AS THIS READING SEES IT: a
 *   functioning global settlement and accounting infrastructure (real
 *   coordination) that simultaneously transfers purchasing power from
 *   currency holders to issuers and first receivers, transferred bargaining
 *   power from labor to mobile capital, and traps official reserve holders
 *   (real, dispersed extraction). Epsilon's referent is that standing
 *   arrangement, assessed by this reading's own lights - not the gold
 *   standard it replaced and not any endorsed alternative. Per the
 *   claim/metric independence rule, the claimed type and the metrics are
 *   authored independently. KEY AGENTS (by structural relationship): -
 *   reserve_currency_issuer_governments: primary beneficiary and rule-writer
 *   (institutional/arbitrage) - collects seigniorage, issues the liability
 *   everyone else must hold - central_banks: administrator
 *   (institutional/identity_locked) - runs the discretionary-management
 *   function, collects authority rather than revenue -
 *   commercial_banking_sector: secondary beneficiary (powerful/mobile) -
 *   earns the spread on elastic credit creation -
 *   internationally_mobile_capital: secondary beneficiary
 *   (powerful/arbitrage) - captures settlement-speed rents -
 *   fixed_income_savers_and_wage_earners: primary target
 *   (moderate/constrained) - bears the inflation tax on wages, deposits,
 *   pensions - creditor_nations_holding_dollar_reserves: trapped target
 *   (institutional/trapped) - bears erosion of the largest official claim
 *   stock; exit is self-punishing - organized_labor: target of the
 *   bargaining-power strand (organized/constrained) - lost wage share across
 *   the transition window - hard_money_advocates: excluded voice
 *   (moderate/constrained) - objects outside the technical conversation -
 *   economic_historians: analytical observer (analytical/analytical) - host
 *   the competing readings and the evidence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.52).
domain_priors:suppression_score(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.47).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__composite_overdetermination_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__composite_overdetermination_reading, "Post-Transition Fiat Monetary Arrangement as Emergent Convergence (Composite Overdetermination Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__composite_overdetermination_reading, "economic/political/historical").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'd029da53-87d1-4103-a658-260e0b5ee5ea').
narrative_ontology:cs_kernel_codification('d029da53-87d1-4103-a658-260e0b5ee5ea', distributed).
narrative_ontology:cs_authority_grounding('d029da53-87d1-4103-a658-260e0b5ee5ea', expertise).
narrative_ontology:cs_interpretation_layer_present('d029da53-87d1-4103-a658-260e0b5ee5ea').
narrative_ontology:cs_reading_relation('d029da53-87d1-4103-a658-260e0b5ee5ea', gold_fiat_transition_mechanism__automatic_constraint_reading, influences).
narrative_ontology:cs_reading_relation('d029da53-87d1-4103-a658-260e0b5ee5ea', gold_fiat_transition_mechanism__creditor_discipline_reading, influences).
narrative_ontology:cs_axiom('d029da53-87d1-4103-a658-260e0b5ee5ea', foundational, transition_is_convergence_not_swap).
narrative_ontology:cs_axiom_status(transition_is_convergence_not_swap, holdable).
narrative_ontology:cs_axiom_grounding('d029da53-87d1-4103-a658-260e0b5ee5ea', transition_is_convergence_not_swap, empirically_contingent).
narrative_ontology:cs_axiom('d029da53-87d1-4103-a658-260e0b5ee5ea', secondary, nixon_shock_symbolic_not_causal).
narrative_ontology:cs_axiom_status(nixon_shock_symbolic_not_causal, holdable).
narrative_ontology:cs_axiom_grounding('d029da53-87d1-4103-a658-260e0b5ee5ea', nixon_shock_symbolic_not_causal, empirically_contingent).
narrative_ontology:cs_reference_frame('d029da53-87d1-4103-a658-260e0b5ee5ea', emergent_convergence_no_unified_transition).
narrative_ontology:cs_drift_state('d029da53-87d1-4103-a658-260e0b5ee5ea', post_qe_and_crypto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d029da53-87d1-4103-a658-260e0b5ee5ea', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__composite_overdetermination_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer_governments).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, commercial_banking_sector).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, internationally_mobile_capital).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers_and_wage_earners).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nations_holding_dollar_reserves).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized_labor).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__composite_overdetermination_reading, monetary_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the currency the rest of the system must hold to settle trade and service debts. Collects seigniorage and borrows in its own money at rates anchored by global reserve demand. Writes and maintains the legal tender statute and tax-acceptance rules for the largest component of the arrangement. Ordinary exit does not apply: it writes the rules under which everyone else would exit.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, reserve_currency_issuer_governments, agenda_setter).

% Creates deposit money through lending and earns the spread on credit expansion that an elastic fiat supply permits. Operates across jurisdictions and can restructure or relocate booking entities when rules tighten in any one of them. Gains arrive as margin, not as a collected levy.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, commercial_banking_sector, beneficiary,
    powerful, biographical, mobile, global).

% Captured the settlement-speed gains first and most fully as telecommunications made cross-border flows instantaneous. Arbitrages interest and exchange differentials across the system in milliseconds and bears almost none of the arrangement's costs, which fall on holders who cannot move.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, internationally_mobile_capital, beneficiary,
    powerful, immediate, arbitrage, global).

% Administer the discretionary monetary management function that replaced convertibility mechanics after the pegs fell. Their institutional legitimacy fused with that management role; abandoning it would dissolve their own mandate. They collect authority and permanence rather than revenue, and they conduct the credibility rituals that substitute for the old mechanical anchor.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__composite_overdetermination_reading, central_banks, beneficiary).

% Hold salaries, deposits, and pensions denominated in the currency and absorb the erosion of nominal claims as prices rise. Wages, taxes, and local commerce are payable only in the domestic unit, so refusing it means converting savings at a discount and re-entering the same system. Daily participation is voluntary in appearance and compulsory in structure.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, fixed_income_savers_and_wage_earners, payer,
    moderate, biographical, constrained, national).

% Accumulate reserve balances as the price of export-led growth and financial stability, and bear the erosion of the largest official stock of claims. Spending down or diversifying reserves depresses the very assets they hold, so exit is self-punishing. They hold a seat in the system but no effective veto over it.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_nations_holding_dollar_reserves, payer,
    institutional, generational, trapped, continental).

% Entered the transition window at peak bargaining power and lost ground as manufacturing restructured, capital gained mobility, and wage indexation broke down during disinflation. The bargaining-power strand of the convergence redistributed income away from labor independently of any monetary policy choice, and labor had no exit from that shift short of political mobilization.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, organized_labor, payer,
    organized, generational, constrained, regional).

% Argue for commodity anchoring or competing private currencies and contend the arrangement rests on compulsion rather than consent. Present in public debate but outside the technical policy forums where the arrangement is actually administered and adjusted.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, hard_money_advocates, excluded,
    moderate, biographical, constrained, national).

% Reconstruct the transition from archives, price series, and institutional records. The profession hosts the competing interpretations of what happened between the late 1960s and the 1990s and supplies the evidence any resolution of those disputes would draw on.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__composite_overdetermination_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally accepted medium of exchange, unit of account, and settlement rail for an economy whose transaction volume and speed outgrew commodity settlement. Component-wise: telecommunications solved cross-border settlement latency; the end of the pegs removed the fixed-supply bottleneck on liquidity; legal tender maturity guaranteed acceptance; and the wage-price flexibility that followed labor's shift absorbed shocks the pegs used to transmit.
% TRANSFER_FUNCTION: Moves purchasing power from currency holders to issuers and first receivers of new credit (seigniorage and the Cantillon path); moved bargaining power from organized labor to mobile capital across the transition window; and moves the settlement-speed advantage to whoever owns the fastest pipes.
% ABSENT_VOICES: Hard-money advocates and cash-dependent households are present in public discourse but absent from the technical forums where the arrangement is maintained; creditor nations hold a seat but no effective veto, since coordinated exit is self-destructive for the exiters.
% DISAPPEARANCE_RATIONALE: Every debt contract, pension obligation, tax liability, and payment rail in the system is denominated in the currencies this arrangement sustains; overnight disappearance would void the nominal layer of the entire economy and force reconstruction around whatever settlement standard survived.
% FOUNDING_PROBLEM: On this reading there was no single founding problem: the arrangement was assembled from solutions to four separate problems - settlement latency (telecommunications), the Triffin dilemma and reserve scarcity (peg collapse), industrial restructuring and wage compression (labor-power shift), and compulsory acceptance for taxation (legal tender maturity). The appearance of a unified founding act is an artifact of the Nixon Shock's visibility.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians working outside the arrangement's benefiting parties corroborate the component-wise record: the clearing and telecommunications literature documents settlement change predating 1971; union-density and strike statistics place the labor shift's onset in the late 1960s; legal tender case law matures decades before the pegs fell. No party inside the arrangement attests the no-single-founder reading, because doing so would dissolve the attribution claims the rival interpretations depend on - corroboration comes from the archival record itself, not from beneficiaries.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__composite_overdetermination_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.52 at interval end): the arrangement performs real transfers - seigniorage to issuers, Cantillon gains to first receivers of credit, an inflation tax on holders - but it also delivers a working global money, and on this reading no single seat captures the gains, which caps how extractive the composite can honestly be scored. Suppression (0.47) is authored as a raw structural property, unscaled by power or scope: legal-tender statutes and tax-acceptance requirements are load-bearing, but they rarely need visible activation because network effects do the daily work, and capital controls - the most openly coercive strand - were largely dismantled over the interval. Theater (0.33): convertibility once anchored confidence mechanically; after 1971 confidence is maintained performatively (target announcements, credibility rhetoric, central-bank communication), a real but partial substitution - clearing and settlement remain functional throughout. Accessibility collapse (0.62): foreign currency, gold, and later crypto exist as conceptual alternatives, but tax liabilities and legal-tender rules collapse most practical exits without eliminating the conceptual ones. Resistance (0.38): recurring hard-money politics and episodic exit attempts, never sustained mass resistance. Claimed type tangled_rope is asserted from structure - genuine coordination plus asymmetric extraction plus active enforcement - independently of these metric values. The measurement series share one grid (eight points, 1968-1990): extractiveness rises through the convergence years then plateaus (the early-1980s disinflation temporarily compressed the inflation tax); theater rises as credibility ritual replaces convertibility mechanics; suppression_requirement declines gently as capital controls are dismantled and enforcement normalizes into background - this story tracks enforcement normalization rather than intensification, which is why the series falls instead of staying flat.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats compute differently from identical structural data. Two institutional actors sit at the same power atom with opposite relationships: the reserve issuer (beneficiary, arbitrage exit - it writes the rules others would exit under) and the creditor nations (payer, trapped exit - diversification destroys their own asset values), so the engine derives opposed directionalities despite equal standing. Among the powerful, mobile capital and commercial banks both face low extraction, but their horizons differ (immediate versus biographical), which changes how each experiences the same arrangement. The administrator seat (central banks) is neither pure beneficiary nor payer: it collects authority and institutional permanence, not revenue, and its identity lock means it cannot evaluate the arrangement apart from its own mandate. Savers and labor, constrained and unable to denominate out of the currency, sit nearest the full-target end among private actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: reserve_currency_issuer_governments derive near-zero directionality (full subsidy side) - they issue the liability, collect seigniorage, and hold arbitrage-grade exit; internationally_mobile_capital similarly near zero; commercial_banking_sector low. Victims: fixed_income_savers_and_wage_earners and organized_labor derive high directionality (constrained exit, must hold the currency); creditor_nations_holding_dollar_reserves derive the highest effective extraction in the story - victims whose trapped exit pushes them toward the full-target end despite institutional power, amplified by the arrangement's global scope, since verification difficulty at scale scales effective extraction upward. Central banks sit mid-low: structural beneficiaries of authority, not of revenue. No directionality_overrides are authored: the available override granularity is the power atom, and this story's institutional seats hold opposed relationships (issuer versus creditor nations versus administrator), so a per-atom override would corrupt three seats to adjust one; the declared beneficiary/victim roles plus exit options give the derivation chain everything it needs.
 *
 * MANDATROPHY ANALYSIS:
 *   The composite reading blocks mandatrophy error in both directions. Reading the whole composite as a snare would require a capturer; the receipt surface here is component-wise (issuers take seigniorage, banks take credit spreads, mobile capital takes arbitrage), so no seat concentrates the gains and the capture tests find nothing to grip. Reading it as a pure rope would erase the real transfers the inflation tax and the labor-share shift represent. The R5 genealogy records the deeper point: on this reading there was no single founding problem and hence no single mandate to outlive its function - the founding-problem status is contested precisely because the rival readings assert founding acts this reading denies. The mismatch consumer reads status=contested against verdict=world_rearranges and finds no zombie signature: the arrangement persists because the world depends on it, not because anyone maintains a dead mandate theatrically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_singularity_dispute,
    'Was the gold-to-fiat transition a single causal event with a unified mechanism (as the automatic-constraint and creditor-discipline readings hold), or a convergence of independent structural changes with no unified transition (this reading)?',
    'Process-tracing and counterfactual analysis of the four candidate strands: if removing any one strand (holding the others fixed) prevents the fiat consolidation, the strands are complements within one transition; if each strand consolidates fiat independently, the siblings'' unified-transition premise fails.',
    'If a unified transition existed, this reading''s diffuse-beneficiary structure misattributes and the sibling readings'' causal nodes regain primacy; if convergence is correct, both siblings misattribute causality to a non-existent unified transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_singularity_dispute, empirical, 'Whether the kernel names one transition or a convergence - the dispute this reading is one side of.').

omega_variable(
    dominant_benefactor_emergence,
    'After convergence, did a dominant beneficiary consolidate around the composite arrangement, or do distributional effects remain dispersed across strands?',
    'Track seigniorage revenue shares, banking-sector rent measures, and capital-flow gains against the inflation tax borne by holders over the post-1990 interval.',
    'A consolidating capturer would push the classification toward captured extraction; persistent dispersion supports the moderate hybrid classification authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominant_benefactor_emergence, empirical, 'Whether the no-single-beneficiary structure held after consolidation or a capturer emerged.').

omega_variable(
    fiat_persistence_naturalness,
    'Is the fiat arrangement''s persistence a natural network effect (any successful money becomes self-sustaining) or does it depend on continuous legal-tender and tax compulsion?',
    'Compare jurisdictions with weak statutory compulsion but strong tax administration against those with strong legal-tender enforcement; observe whether dollarization and crypto adoption track enforcement intensity.',
    'If network effects alone sustain it, suppression is overstated and the arrangement sits nearer pure coordination; if compulsion is load-bearing, the hybrid classification with active enforcement holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiat_persistence_naturalness, conceptual, 'Natural network effect versus enforced compulsion as the persistence mechanism.').

omega_variable(
    nixon_shock_counterfactual_status,
    'In the counterfactual where the August 1971 announcement does not occur, does the fiat consolidation happen anyway through the other strands, on this reading''s own timeline?',
    'Archival reconstruction of the parallel decisions already underway (currency floats, clearing-network buildout, wage settlements) to establish whether the announcement merely marked a consolidation in progress.',
    'Confirms or refutes the symbolic-not-causal axiom; a finding that the announcement was decisive would collapse this reading into the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nixon_shock_counterfactual_status, empirical, 'Counterfactual status of the 1971 announcement as marker versus cause.').

omega_variable(
    composite_decomposition_boundary,
    'Does the composite arrangement''s epsilon remain invariant across observables (inflation tax, seigniorage, Cantillon gains measured separately), or do the strands warrant decomposition into separate constraint stories?',
    'Compute epsilon per strand; if strand-level values diverge widely and carry distinct beneficiary and victim sets, split into a constraint family linked by network edges.',
    'Decomposition would replace this single moderate-epsilon story with a family (legal-tender compulsion, seigniorage arrangement, capital-mobility regime), each with its own classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(composite_decomposition_boundary, conceptual, 'Whether the composite is one epsilon-invariant constraint or a family awaiting decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__composite_overdetermination_reading, 1968, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftr_composite_tr_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1968, 0.14).
narrative_ontology:measurement_basis(gftr_composite_tr_t1968, observed).
narrative_ontology:measurement(gftr_composite_tr_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1971, 0.17).
narrative_ontology:measurement_basis(gftr_composite_tr_t1971, observed).
narrative_ontology:measurement(gftr_composite_tr_t1974, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1974, 0.21).
narrative_ontology:measurement_basis(gftr_composite_tr_t1974, observed).
narrative_ontology:measurement(gftr_composite_tr_t1977, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1977, 0.24).
narrative_ontology:measurement_basis(gftr_composite_tr_t1977, observed).
narrative_ontology:measurement(gftr_composite_tr_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1980, 0.27).
narrative_ontology:measurement_basis(gftr_composite_tr_t1980, observed).
narrative_ontology:measurement(gftr_composite_tr_t1983, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1983, 0.31).
narrative_ontology:measurement_basis(gftr_composite_tr_t1983, observed).
narrative_ontology:measurement(gftr_composite_tr_t1986, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1986, 0.33).
narrative_ontology:measurement_basis(gftr_composite_tr_t1986, observed).
narrative_ontology:measurement(gftr_composite_tr_t1990, gold_fiat_transition_mechanism__composite_overdetermination_reading, theater_ratio, 1990, 0.33).
narrative_ontology:measurement_basis(gftr_composite_tr_t1990, observed).

% Extraction over time
narrative_ontology:measurement(gftr_composite_be_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement_basis(gftr_composite_be_t1968, observed).
narrative_ontology:measurement(gftr_composite_be_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement_basis(gftr_composite_be_t1971, observed).
narrative_ontology:measurement(gftr_composite_be_t1974, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1974, 0.47).
narrative_ontology:measurement_basis(gftr_composite_be_t1974, observed).
narrative_ontology:measurement(gftr_composite_be_t1977, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1977, 0.5).
narrative_ontology:measurement_basis(gftr_composite_be_t1977, observed).
narrative_ontology:measurement(gftr_composite_be_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1980, 0.53).
narrative_ontology:measurement_basis(gftr_composite_be_t1980, observed).
narrative_ontology:measurement(gftr_composite_be_t1983, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1983, 0.51).
narrative_ontology:measurement_basis(gftr_composite_be_t1983, observed).
narrative_ontology:measurement(gftr_composite_be_t1986, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1986, 0.51).
narrative_ontology:measurement_basis(gftr_composite_be_t1986, observed).
narrative_ontology:measurement(gftr_composite_be_t1990, gold_fiat_transition_mechanism__composite_overdetermination_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(gftr_composite_be_t1990, observed).

% Suppression requirement over time
narrative_ontology:measurement(gftr_composite_su_t1968, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement_basis(gftr_composite_su_t1968, observed).
narrative_ontology:measurement(gftr_composite_su_t1971, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1971, 0.52).
narrative_ontology:measurement_basis(gftr_composite_su_t1971, observed).
narrative_ontology:measurement(gftr_composite_su_t1974, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1974, 0.55).
narrative_ontology:measurement_basis(gftr_composite_su_t1974, observed).
narrative_ontology:measurement(gftr_composite_su_t1977, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1977, 0.54).
narrative_ontology:measurement_basis(gftr_composite_su_t1977, observed).
narrative_ontology:measurement(gftr_composite_su_t1980, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement_basis(gftr_composite_su_t1980, observed).
narrative_ontology:measurement(gftr_composite_su_t1983, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1983, 0.49).
narrative_ontology:measurement_basis(gftr_composite_su_t1983, observed).
narrative_ontology:measurement(gftr_composite_su_t1986, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1986, 0.47).
narrative_ontology:measurement_basis(gftr_composite_su_t1986, observed).
narrative_ontology:measurement(gftr_composite_su_t1990, gold_fiat_transition_mechanism__composite_overdetermination_reading, suppression_requirement, 1990, 0.47).
narrative_ontology:measurement_basis(gftr_composite_su_t1990, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__composite_overdetermination_reading, global_infrastructure).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, automatic_constraint_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__composite_overdetermination_reading, creditor_discipline_reading).

% DUAL FORMULATION NOTE:
% Constraint family: three readings of one kernel, each a separate story with its own epsilon, beneficiary structure, and classification. The automatic_constraint_reading is the upstream baseline (the most established narrative, cited as settled context by the others); this composite reading and the creditor_discipline_reading are downstream challengers. Edges here run FROM this reading TO both siblings because its convergence thesis pressures their attribution claims - changing their legitimacy conditions - without logically foreclosing either: a party could in principle hold that a material constraint ended AND that the ending was one strand of a broader convergence. If the composite reading resolves true, the family's upstream/downstream ordering inverts: the siblings become special cases that over-weighted a single strand of a convergence they mistook for the whole event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
