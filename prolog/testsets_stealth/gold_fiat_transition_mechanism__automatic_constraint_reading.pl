% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Post-1971 Discretionary Fiat Monetary Regime (Automatic-Constraint Reading)
 *   domain: economic/political/monetary_history
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. Between 1971
 *   and 1976 the advanced-economy monetary order abandoned reserve
 *   convertibility: the automatic, material ceiling that gold reserves had
 *   placed on money creation was dismantled (closure of the gold window,
 *   failure of the Smithsonian realignment, the float, and the Jamaica
 *   Accords' formalization), leaving discretionary central bank authority as
 *   the operative limit on issuance. On the automatic_constraint_reading,
 *   this is a constraint-type substitution — material to institutional — and
 *   the standing arrangement under analysis is the post-transition
 *   discretionary regime itself. Its epsilon is authored for that standing
 *   arrangement as this reading sees it: high, because the automatic ceiling
 *   on extraction is gone and the historical record shows repeated expansive
 *   episodes (1970s inflation, post-2008 balance-sheet expansion, the
 *   post-2020 surge) bounded only by mandate, politics, and market reaction.
 *   KEY AGENTS (by structural relationship): - monetary_authorities: Agenda
 *   setter (institutional/arbitrage) — administers issuance, gained
 *   discretion - government_treasuries: Beneficiary
 *   (institutional/constrained) — receives seigniorage and real-debt erosion
 *   - creditor_class: Primary target among organized wealth-holders
 *   (powerful/constrained) — lost automatic protection - fixed_income_savers:
 *   Primary target among the trapped (powerless/trapped) — absorbs the
 *   transfer unhedged - net_debtors: Incidental beneficiary
 *   (moderate/constrained) - hard_money_advocates: Excluded voice
 *   (moderate/trapped) - monetary_economists: Analytical observer — sees the
 *   full structure Sibling readings of the same kernel are separate
 *   constraint stories linked via network.affects_constraints; their content
 *   enters this file only through the kernel omega.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.71).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.62).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Post-1971 Discretionary Fiat Monetary Regime (Automatic-Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "economic/political/monetary_history").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '5a1f9d43-5970-4d99-b5c4-51ec56c2cd78').
narrative_ontology:cs_kernel_codification('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', formalized).
narrative_ontology:cs_authority_grounding('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', extraction).
narrative_ontology:cs_interpretation_layer_present('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78').
narrative_ontology:cs_reading_relation('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', foundational, automatic_material_limits_preferable_to_discretion).
narrative_ontology:cs_axiom_status(automatic_material_limits_preferable_to_discretion, holdable).
narrative_ontology:cs_axiom_grounding('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', automatic_material_limits_preferable_to_discretion, instrumental).
narrative_ontology:cs_axiom('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', secondary, discretion_carries_structural_inflation_bias).
narrative_ontology:cs_axiom_status(discretion_carries_structural_inflation_bias, holdable).
narrative_ontology:cs_axiom_grounding('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', discretion_carries_structural_inflation_bias, empirically_contingent).
narrative_ontology:cs_reference_frame('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', reserve_convertibility_automaticity).
narrative_ontology:cs_drift_state('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', post_jamaica_accords_contemporary, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('5a1f9d43-5970-4d99-b5c4-51ec56c2cd78', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, government_treasuries).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, net_debtors).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, legal_tender_enforcement_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, countercyclical_stabilization_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the policy rate, issue the currency, and decide the size of the balance sheet. Since 1971 no reserve-convertibility obligation bounds these choices; the binding limits are statutory mandates, political accountability, and market reaction. They remit seigniorage earnings to treasuries and act as lender of last resort in crises. Exit is meaningless in the ordinary sense because they administer the system, but they retain unmatched discretion to redefine their own operating frameworks — as with inflation-targeting reforms — without renegotiating with the holders of the currency.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Finance deficits by issuing nominal debt in a currency their central bank issues; inflation erodes the real value of that debt and seigniorage remittances supplement tax revenue. They cannot credibly commit to future restraint without surrendering the flexibility that deficit finance purchases, and their access to the inflation channel depends on staying aligned with the monetary authority.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, government_treasuries, beneficiary,
    institutional, biographical, constrained, national).

% Hold bonds, loans, and other nominal claims. Before 1971 a gold-redemption threat capped issuer discretion; afterward, repayment arrives in whatever purchasing power the issuer's policy leaves. Large holders can demand inflation premia, buy indexed instruments, or shift jurisdictions, but every reinvestment destination prices in the same issuer discretion — exit means accepting lower yield or bearing someone else's regime.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Hold wages, deposits, pensions, and annuities denominated in the national unit of account. Taxes and debts are payable only in that unit; switching to hard assets or foreign currency carries friction and tax events, and for wage income no substitute exists at all. Purchasing power erodes whenever policy runs expansively, and they hold no seat in the committees that decide.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Owe mortgages, business loans, and other nominal debts; expansive policy shrinks the real burden of repayment. They benefit passively — they did not design the regime and cannot expand it further on their own behalf — and their gain is the mirror image of their creditors' loss.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, net_debtors, beneficiary,
    moderate, biographical, constrained, national).

% Sound-money movements, gold-standard proponents, and later private digital-currency advocates argue for restoring automatic limits on issuance. Competing-currency proposals are legally foreclosed by legal-tender statutes and tax treatment; their advocacy persists outside the policy conversation, consulted rarely and overruled routinely.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, hard_money_advocates, excluded,
    moderate, biographical, trapped, global).

% Academic and central-bank research communities, the BIS, and economic historians analyze the regime's record — inflation persistence, credibility cycles, time-inconsistency — and publish evaluations that occasionally reshape operating frameworks but hold no enforcement seat.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an elastic medium of exchange and lender-of-last-resort capacity: the money supply can expand in crises to prevent liquidity collapses and deflationary spirals, seigniorage funds public expenditure, and the unit of account is managed actively rather than held to a metal stock.
% TRANSFER_FUNCTION: Moves purchasing power from holders of nominal claims — creditors, savers, wage earners — to issuers and debtors via inflation and seigniorage whenever discretion is exercised expansively; moves crisis-response capacity to the monetary center.
% ABSENT_VOICES: Hard-money advocates and unindexed savers are outside the policy conversation. Their objection — that discretion will predictably be abused and that only an automatic constraint protects nominal claimholders — was voiced before 1971 and dismissed, and no seat inside the arrangement represents the automatic-constraint position; the closest internal proxies (inflation-targeting hawks) operate entirely within the discretionary framework they would constrain.
% DISAPPEARANCE_RATIONALE: Every nominal contract, wage schedule, price list, tax code, and sovereign debt stock in the advanced economies is denominated in discretionary fiat. Overnight removal of the regime would force either a new anchor — restoring some convertibility or rule — or a chaotic repricing of all nominal claims; wage-setting, fiscal capacity, and the lender-of-last-resort backstop would all rearrange.
% FOUNDING_PROBLEM: The Bretton Woods convertibility bind: the dollar-gold peg strained by US deficits and speculative attacks, with the Triffin dilemma and the system's deflationary bias preventing domestic stabilization responses. The transition was built to escape that bind.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: contemporaneous congressional testimony and the memoir record of Fed leadership (Arthur Burns) acknowledging the convertibility bind; IMF archives documenting the 1971-73 collapse mechanics; retrospective economic history (Eichengreen; Bordo and Eichengreen) attesting the par-value system was unsustainable. No external source corroborates that the successor discretionary mandate was a designed solution rather than an improvised escape — the improvisation record is itself evidence the founding problem was fled, not solved, which is why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction opens high (0.66 at t0): the first post-transition decade delivered double-digit inflation, the fastest peacetime transfer from nominal claimholders on record. Volcker's disinflation and the Great Moderation restored credibility, driving extraction to its trough (0.52 at t20); each subsequent crisis cycle (GFC-era QE, pandemic-era expansion) re-expanded the balance sheet faster than the last and ended at a higher plateau than it began (0.71 at t50) — a ratchet, not a random walk. Theater rises monotonically (0.14 to 0.36): as realized inflation control weakened relative to communication, forward guidance, target-framework revisions, and 'transitory' diagnostics absorbed a growing share of institutional activity. Suppression_requirement climbs gently (0.46 to 0.62), tracking maturation of legal-tender enforcement, tax-code entrenchment of the unit of account, and payment-system surveillance — the coercive infrastructure hardened even as overt coercion stayed low. Suppression is authored as a raw structural property and is deliberately NOT scaled by scope or directionality; only extractiveness is engine-scaled. Accessibility_collapse (0.44) is moderate: indexation, hard assets, foreign currency, and private digital alternatives remain reachable, so alternatives persist but none displaces the unit of account for taxes and debts. Resistance (0.52) is organized at the top of the wealth distribution (indexation, premia, jurisdiction shopping) and diffuse below it. Claimed type is tangled_rope on the reading's own lights — genuine stabilization coordination (elastic currency, lender of last resort) and asymmetric extraction run through the same structure and require active enforcement — while the metrics are authored independently as descriptive facts; the engine computes per-seat classifications from the structural data. All three series share one time grid (t=0,10,20,30,40,50) so no metric row borrows an end-state value from another.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat the regime is a stabilization toolkit won at great cost (the Volcker recession) and continuously defended; from the payer seats the same structure is an unbounded debasement option with its ceiling removed. Same-level divergence matters more than power divergence here: creditor_class (powerful, partially hedged) defends through pricing and jurisdiction shifts, while fixed_income_savers (powerless, trapped) absorb the identical nominal exposure without defense — so the engine should compute materially different effective extraction for the two payer seats despite both being declared victims. Coalition potential among the powerless is weak: savers are atomized by construction, and their natural coalition partner (organized creditors) has already bought partial protection for itself, which is precisely why the transfer persists.
 *
 * DIRECTIONALITY LOGIC:
 *   monetary_authorities sit nearest the beneficiary pole: the regime subsidizes their discretion directly and they write the rules. government_treasuries sit near-beneficiary: seigniorage and debt erosion flow to them, offset by dependence on the authority. net_debtors are mild passive beneficiaries with no control lever. creditor_class sits near the target pole: it bears the transfer with partial hedging capacity (constrained exit damps d short of full target). fixed_income_savers sit at the full-target end: trapped, unhedged, no seat. hard_money_advocates are excluded rather than coordinated — they stand outside the benefit/cost flows, objecting from outside. The beneficiary/victim declarations plus the exit asymmetry (constrained vs trapped) already separate the two payer seats, so no directionality_overrides entries were needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview locates the mandatrophy question precisely: the founding problem (the Bretton Woods convertibility bind) is dead — no par-value system remains to strain — yet the arrangement persists under a successor stabilization mandate whose necessity the parties dispute. Authoring founding_problem_status='contested' against disappearance_verdict='world_rearranges' flags the live dispute without asserting zombie status: unlike a piton, the arrangement is actively administered and its coordination function demonstrably operates. The tangled_rope classification prevents both mislabels: reading the regime as pure snare erases the real stabilization coordination that repeatedly prevented deflationary collapses; reading it as pure rope erases the asymmetric transfer that motivated the transition in the first place. Mandatrophy resolution therefore turns on the omega variables — especially whether the successor mandate is a legitimate evolution or a cover for retained discretion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the automatic_constraint_reading of kernel gold_fiat_transition_mechanism: is the post-1971 regime best modeled as replacement of an automatic material constraint by discretionary institutional authority (this reading), as elimination of creditor veto power enabling debtor-nation flexibility (creditor_discipline_reading), or as convergence of independent structural changes in which the Nixon Shock was merely symbolic (composite_overdetermination_reading)?',
    'Comparative counterfactual analysis: test whether a credible automatic constraint could have survived the telecommunications and capital-flow changes alone; trace whether creditor-protection outcomes track constraint type or bargaining-power shifts across countries and decades.',
    'Sibling readings redistribute the victim set (this reading: creditors and trapped savers; creditor reading: creditor nations as geopolitical actors; composite reading: no concentrated victim) and move epsilon; the standing regime classifies as tangled_rope under this reading but approaches rope under the composite reading, where discretion is one co-evolving element among several.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the gold-fiat transition kernel correctly models the standing arrangement.').

omega_variable(
    automaticity_prior_status,
    'Was the pre-1971 gold constraint genuinely automatic and material, or already a suspendable institutional commitment (suspended 1914, 1931, 1933, 1968) whose automaticity was itself maintained by convention?',
    'Historical analysis of the suspension episodes and the legal architecture of convertibility commitments: if convertibility was routinely suspended under stress, the ''physical'' constraint was already institutional, and 1971 changed degree rather than kind.',
    'If the prior constraint was already institutional, the transition''s structural delta shrinks — the standing arrangement inherits a longer institutional pedigree, epsilon attribution to the 1971 break falls, and the material-to-institutional framing that defines this reading loses force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automaticity_prior_status, empirical, 'Whether the eliminated constraint was truly material-automatic or already institutionally contingent.').

omega_variable(
    surprise_inflation_extraction_share,
    'How much of the measured extraction is unexpected inflation (a pure transfer) versus anticipated inflation already priced into nominal interest rates (a traded risk)?',
    'Decompose realized inflation into expected and surprise components using ex-ante survey and market-based expectations series; attribute extraction to the surprise residual.',
    'If most post-1971 inflation was anticipated and priced, effective extraction on sophisticated creditors falls sharply and concentrates instead on trapped nominal-wage earners and cash holders; the epsilon composition shifts across victim seats even if the aggregate holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surprise_inflation_extraction_share, empirical, 'Share of extraction constituted by surprise inflation versus priced inflation risk.').

omega_variable(
    regime_counterfactual_extraction,
    'How much post-1971 extraction is attributable to loss of the automatic constraint versus fiscal pressures that would have forced similar outcomes under any regime?',
    'Cross-country panel comparing discretionary floaters against rule-bound regimes (currency boards, hard pegs) facing comparable fiscal stress; measure debasement-or-default outcomes conditional on fiscal stance.',
    'If rule-bound regimes under similar fiscal stress show comparable losses (via default instead of inflation), the discretionary regime''s incremental epsilon is smaller than headline inflation suggests — extraction substitutes form rather than magnitude, and this reading''s high-epsilon claim narrows accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_counterfactual_extraction, empirical, 'Incremental extraction attributable to discretion versus fiscal dominance common to all regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftr_auto_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(gftr_auto_tr_t0, observed).
narrative_ontology:measurement(gftr_auto_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(gftr_auto_tr_t10, observed).
narrative_ontology:measurement(gftr_auto_tr_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(gftr_auto_tr_t20, observed).
narrative_ontology:measurement(gftr_auto_tr_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(gftr_auto_tr_t30, observed).
narrative_ontology:measurement(gftr_auto_tr_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement_basis(gftr_auto_tr_t40, observed).
narrative_ontology:measurement(gftr_auto_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.36).
narrative_ontology:measurement_basis(gftr_auto_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gftr_auto_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement_basis(gftr_auto_be_t0, observed).
narrative_ontology:measurement(gftr_auto_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(gftr_auto_be_t10, observed).
narrative_ontology:measurement(gftr_auto_be_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement_basis(gftr_auto_be_t20, observed).
narrative_ontology:measurement(gftr_auto_be_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 30, 0.57).
narrative_ontology:measurement_basis(gftr_auto_be_t30, observed).
narrative_ontology:measurement(gftr_auto_be_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(gftr_auto_be_t40, observed).
narrative_ontology:measurement(gftr_auto_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.71).
narrative_ontology:measurement_basis(gftr_auto_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gftr_auto_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement_basis(gftr_auto_su_t0, observed).
narrative_ontology:measurement(gftr_auto_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(gftr_auto_su_t10, observed).
narrative_ontology:measurement(gftr_auto_su_t20, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(gftr_auto_su_t20, observed).
narrative_ontology:measurement(gftr_auto_su_t30, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 30, 0.56).
narrative_ontology:measurement_basis(gftr_auto_su_t30, observed).
narrative_ontology:measurement(gftr_auto_su_t40, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement_basis(gftr_auto_su_t40, observed).
narrative_ontology:measurement(gftr_auto_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(gftr_auto_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'end of the gold standard' decomposes into at least three structurally distinct claims (constraint family): this story models the constraint-type substitution (material to institutional) and authors high epsilon for the resulting discretionary arrangement; creditor_discipline_reading models the same transition as vaporization of creditor veto power with a geopolitical victim set; composite_overdetermination_reading denies a single causal node entirely. Each story carries its own epsilon, beneficiaries, and victims; they are linked here because the upstream claim (what the automatic constraint was) conditions the downstream claims (who lost what).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
