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
 *   human_readable: Discretionary Fiat Money Authority (Automatic-Constraint Reading)
 *   domain: economic/political/historical
 *
 * SUMMARY:
 *   This story instantiates the automatic-constraint reading of the 1971
 *   monetary transition. On this reading, before August 1971 the supply of
 *   reserve-currency money was bounded by an automatic material mechanism:
 *   the unit was convertible into gold at fixed parity, so base money was
 *   capped by the monetary gold stock and the redemption commitments
 *   outstanding against it. The constraint enforced itself — no committee had
 *   to decide to obey it, because breach triggered mechanical gold outflows.
 *   The transition replaced that material mechanism with discretionary
 *   central bank authority: the unit's supply is now set by committee
 *   decision within statutory mandates, bounded by nothing external to the
 *   issuing institution. The standing arrangement under contest — and
 *   therefore the epsilon referent for this story — is the post-transition
 *   discretionary regime itself, assessed by this reading's lights: a
 *   constraint that weakened from material to institutional, whose slack
 *   accrues to the issuing complex and whose risks fall on holders of nominal
 *   claims. KEY AGENTS (by structural relationship): - monetary_authorities:
 *   Agenda setter (institutional/arbitrage) — sets the unit's supply,
 *   collects the seigniorage spread - sovereign_fiscal_authorities: Secondary
 *   beneficiary (institutional/arbitrage) — monetizable deficits,
 *   remittances, inflation haircut on debt - commercial_banking_system:
 *   Beneficiary (organized/constrained) — deposit expansion on elastic
 *   reserves, crisis backstop - creditor_class: Primary target
 *   (powerful/constrained) — long nominal claims without specie recourse -
 *   fixed_income_savers: Primary target (powerless/trapped) — unhedged
 *   debasement exposure - hard_money_advocates: Excluded voice
 *   (moderate/constrained) — anchor-restoration program kept outside
 *   governance - monetary_regime_analysts: Analytical observer
 *   (analytical/analytical) — documents the substitution
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.74).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.57).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Discretionary Fiat Money Authority (Automatic-Constraint Reading)").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "economic/political/historical").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '52a11afa-8cd8-4013-ba8f-6356544e699f').
narrative_ontology:cs_kernel_codification('52a11afa-8cd8-4013-ba8f-6356544e699f', formalized).
narrative_ontology:cs_authority_grounding('52a11afa-8cd8-4013-ba8f-6356544e699f', expertise).
narrative_ontology:cs_interpretation_layer_present('52a11afa-8cd8-4013-ba8f-6356544e699f').
narrative_ontology:cs_reading_relation('52a11afa-8cd8-4013-ba8f-6356544e699f', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('52a11afa-8cd8-4013-ba8f-6356544e699f', gold_fiat_transition_mechanism__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('52a11afa-8cd8-4013-ba8f-6356544e699f', foundational, automatic_constraints_admit_no_discretionary_substitute).
narrative_ontology:cs_axiom_status(automatic_constraints_admit_no_discretionary_substitute, holdable).
narrative_ontology:cs_axiom_grounding('52a11afa-8cd8-4013-ba8f-6356544e699f', automatic_constraints_admit_no_discretionary_substitute, empirically_contingent).
narrative_ontology:cs_axiom('52a11afa-8cd8-4013-ba8f-6356544e699f', secondary, discretion_requires_compensating_accountability).
narrative_ontology:cs_axiom_status(discretion_requires_compensating_accountability, holdable).
narrative_ontology:cs_axiom_grounding('52a11afa-8cd8-4013-ba8f-6356544e699f', discretion_requires_compensating_accountability, instrumental).
narrative_ontology:cs_reference_frame('52a11afa-8cd8-4013-ba8f-6356544e699f', automatic_gold_reserve_proportional_issue).
narrative_ontology:cs_drift_state('52a11afa-8cd8-4013-ba8f-6356544e699f', contemporary_fiat_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('52a11afa-8cd8-4013-ba8f-6356544e699f', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_authorities).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banking_system).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, discretionary_monetary_policy_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, lender_of_last_resort_necessity).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, elastic_currency_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the supply of the monetary unit by committee decision within statutory mandates; issues base money, sets policy rates, and purchases assets. Collects the seigniorage spread between the near-zero cost of creating the unit and the interest-bearing assets acquired with it. Faces no external redemption obligation; the binding limits are the mandates it interprets and the political principals who appoint its leadership. Ordinary exit does not apply — it defines the unit everyone else must use — but it can shift costs onto other seats at will by changing the supply.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Issues the debt that the monetary arm purchases and upon which unanticipated inflation acts as a silent haircut. Gains financing flexibility: deficits can be rolled and absorbed where a specie-proportional regime would have forced taxation or default. Receives central-bank remittances. Does not operate the money press directly but collects much of what the press produces.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, sovereign_fiscal_authorities, beneficiary,
    institutional, generational, arbitrage, national).

% Creates deposit money on top of central-bank reserves and lends at spreads over the policy rate. Benefits from elastic reserves in crises — the discount window stands behind it where a specie regime offered no comparable backstop. Pays for the privilege through regulation and reserve requirements it helps shape.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, commercial_banking_system, beneficiary,
    organized, biographical, constrained, global).

% Holds long-duration nominal claims — bonds, mortgage receivables, contracted annuities — denominated in the managed unit. Before 1971 a creditor could threaten redemption into specie; now repayment arrives in whatever purchasing power committee decisions leave the unit with. Can hedge by shifting into real assets, foreign currency, or inflation-linked instruments, but domestic contracts, taxes, and courts all settle in the unit, so the hedge is partial and carries its own costs.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, biographical, constrained, global).

% Holds retirement accounts, bank deposits, and nominal pensions. Lacks the scale, information, or market access to hedge systematically; inflation reaches them through prices while their claims stay fixed. Moving abroad or into illiquid real assets is costly and risky; daily life is transacted in the unit being managed.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, fixed_income_savers, payer,
    powerless, biographical, trapped, national).

% Argue for restoring a specie or rule-bound anchor and characterize discretionary issue as legalized debasement. Organize episodically — gold commissions, sound-money candidacies, monetary-alternative movements — but sit outside operational monetary governance, which is seated with the issuing committees and their appointed staff.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, hard_money_advocates, excluded,
    moderate, generational, constrained, national).

% Academic and official-sector economists who study the regime's operation: measuring inflation transfers, modeling mandate trade-offs, documenting the substitution of committee judgment for material limits. Neither collects nor pays; publishes the record the other seats argue over.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_regime_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an elastic monetary medium: the unit's supply adjusts to transactional demand and crisis liquidity needs instead of being pinned to mine output; enables a lender of last resort; removes the deflationary bias a growing economy suffered under specie-proportional issue.
% TRANSFER_FUNCTION: Moves real purchasing power from holders of nominal claims to the issuing complex and the first receivers of new money — via the seigniorage spread, unanticipated inflation eroding fixed claims, and financial repression holding rates below inflation; secondarily from private creditors to debtors, including the state.
% ABSENT_VOICES: Hard-money advocates are outside the policy conversation except as episodic political pressure; future holders of today's nominal claims — later cohorts who will receive the managed unit as wages, pensions, and debt service — have no seat at all; unorganized small savers lack representation in mandate-setting.
% DISAPPEARANCE_RATIONALE: Overnight removal of discretionary issue would strand every nominal contract, freeze the banking system's reserve hierarchy, and force immediate renegotiation of state finances — the payments architecture of the world economy is built on the arrangement and cannot run without some successor rule for the unit's supply.
% FOUNDING_PROBLEM: Specie-proportional issue starved growing economies of money (deflationary bias), left panics without a lender of last resort (1907, 1930-33), and imported foreign shocks through fixed convertibility — the interwar collapse and the straining of Bretton Woods parities were the proximate triggers for replacing the material limit with committee authority.
% FOUNDING_PROBLEM_CORROBORATION: Monetary-economic history outside the benefiting parties corroborates the founding problem: the scholarly literature on interwar gold-standard constraints, contemporaneous records of the 1930s banking collapses, and international-financial-institution analyses of deflationary bias all attest the original rigidity. Hard-money critics corroborate that the problem was real while disputing that the current degree of discretion is its solution — the problem's existence is corroborated; the remedy's calibration is contested.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.74 at interval end) because the transfer channels — seigniorage spread, unanticipated inflation against fixed claims, financial repression below-inflation rate floors — are decoupled from any external anchor and have been exercised at increasing scale, peaking with the 2020 fiscal-monetary fusion. Suppression (0.57) is a raw structural property, unscaled by power or scope: legal tender status, tax receivability in the unit, and regulatory friction on monetary alternatives give the arrangement its coercive teeth. Theater (0.33) reflects a growing share of activity in forward guidance, mandate proliferation, and communication strategy that manages perception rather than the unit itself. Accessibility collapse (0.62) is moderate-high: domestic transactional alternatives are effectively closed (contracts, taxes, and courts settle in the unit) while store-of-value exits into real assets and foreign currency remain open but costly. Resistance (0.42) is persistent but episodic: hard-money politics, the episodic gold commissions, and monetary-alternative movements recur without displacing the arrangement. The temporal series share one grid (1971-2024, eight points, all three metrics authored at every point). Two dynamics deserve note: base extractiveness dips at 1990 — the credibility-restoration period lowered practiced extraction even though the capacity persisted — and suppression_requirement traces a U-shape: high early enforcement defending the newly unconstrained unit (capital controls, legal tender defense against residual redemption expectations), normalization through the Great Moderation, then re-tightening after 2008 as financial repression and regulatory friction on alternatives rebuilt the enforcement perimeter.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat the arrangement looks like prudential self-governance — the committee experiences the mandate, not an external limit, and its horizon is generational; from that seat the structure presents as near-pure coordination it administers. From the trapped saver seat the same structure presents as pure debasement exposure with no exit. The creditor_class seat sits between: powerful enough to hedge partially, constrained enough to remain exposed. The engine computes these divergent per-seat classifications from the power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations cluster at the low-d end: monetary_authorities define the unit and collect the spread (derived d near the beneficiary pole, amplified by arbitrage-grade exit — they face no external constraint at all); sovereign_fiscal_authorities receive remittances and debt haircuts; commercial_banking_system rides elastic reserves. Victim declarations drive the high-d end: creditor_class bears the transfer with only partial hedging (constrained exit pushes toward, not to, the full-target pole), and fixed_income_savers — trapped, powerless — sit nearest the full-target end. The excluded hard_money_advocates seat marks the suppressed alternative rather than feeding the derivation. Scope amplification applies modestly: the arrangement operates at global scope for the reserve unit, raising verification difficulty and thus effective extraction for the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — specie-proportional rigidity, deflationary bias, absent lender of last resort — remains live: elastic issue still solves it daily, visibly in the 2008 and 2020 liquidity backstops. Mandatrophy is therefore not resolved, and the tangled_rope claim is what keeps both faces of the arrangement visible at once. A pure-extraction reading would erase the real stabilization function the arrangement performs; a pure-coordination reading would erase the asymmetric transfer running through the same structure — the Cantillon advantage of first receivers, the silent haircut on fixed claims, the repression of savers. The classification holds the coordination function and the extraction asymmetry in a single structure, which is exactly what the historical record shows: the same committee discretion that backstops panics also finances deficits at creditors' expense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_transition_kernel,
    'This story is one reading of kernel gold_fiat_transition_mechanism. Would instantiating the creditor_discipline_reading or the composite_overdetermination_reading instead change the constraint''s epsilon, beneficiary structure, or computed type?',
    'Author the sibling stories and compare computed classifications. The disagreement is located in which structural element carries the classification load: the constraint-kind substitution (this reading), the veto-power removal and geopolitical shift (creditor reading), or the absence of any single causal node (composite reading).',
    'Under the creditor reading the victim seat shifts toward foreign reserve holders and the beneficiary toward the reserve-issuer state as a geopolitical actor; under the composite reading epsilon disperses across multiple upstream constraints and no single seat captures the gains. The classification in this file is valid only for the automatic-constraint reading; cross-reading comparison requires the sibling files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_transition_kernel, conceptual, 'Kernel-level indexicality: which reading of the transition is instantiated determines the epsilon referent and seat structure.').

omega_variable(
    fiscal_dominance_vs_independence,
    'Do monetary authorities exercise net discretion as principals, or do they execute fiscal dominance — with the sovereign fiscal arm as the true capturer and central-bank independence operating partly as performance?',
    'Observe remittance behavior under deficit stress, debt-monetization episodes, appointment politics around tightening votes, and whether tightening ever durably precedes fiscal need rather than following it.',
    'If fiscal dominance holds, the receipt surface shifts toward sovereign_fiscal_authorities, the agenda_setter role of monetary_authorities becomes partly nominal, and theater_ratio rises (independence as managed appearance). If independence is real, the authored receipt surface stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_dominance_vs_independence, empirical, 'Whether the issuing committee is principal or agent of the fiscal arm.').

omega_variable(
    creditor_exit_stratification,
    'Is the creditor_class a coherent victim seat, or is it bifurcating — sophisticated capital hedging into real assets, foreign currency, and inflation-linked instruments while unhedged retail savers absorb the residual transfer?',
    'Wealth-composition analysis by decile of inflation-hedge adoption across the interval; measure the correlation between holder sophistication and realized real returns on nominal claims.',
    'If stratification deepens, the effective directionality of creditor_class falls (partial arbitrage) while fixed_income_savers moves nearer the full-target pole — extraction concentrating on the trapped seat and the victim structure becoming two-tier rather than unified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_exit_stratification, empirical, 'Whether hedging capacity splits the payer side into arbitraging and trapped tiers.').

omega_variable(
    reversibility_of_material_anchor,
    'Is the elimination of the automatic material constraint irreversible, or could a crisis restore a rule-bound or commodity-anchored regime — re-materializing the constraint on issue?',
    'Historical base rates: hyperinflation episodes have repeatedly triggered return to specie or foreign anchors. Assess whether any credible restoration path exists given current debt stocks, derivative exposures, and the absence of a remonetized commodity stock.',
    'If reversible, the discretionary arrangement reads as transitional support pending re-anchoring, and its sunset question becomes live; if irreversible, the institutional constraint is the permanent settlement and the material limit is historically closed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_material_anchor, conceptual, 'Whether the material-to-institutional substitution admits a reverse path.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 1971, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1971, 0.14).
narrative_ontology:measurement_basis(gold_tr_t1971, observed).
narrative_ontology:measurement(gold_tr_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(gold_tr_t1980, observed).
narrative_ontology:measurement(gold_tr_t1990, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement_basis(gold_tr_t1990, observed).
narrative_ontology:measurement(gold_tr_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(gold_tr_t2000, observed).
narrative_ontology:measurement(gold_tr_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(gold_tr_t2008, observed).
narrative_ontology:measurement(gold_tr_t2015, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement_basis(gold_tr_t2015, observed).
narrative_ontology:measurement(gold_tr_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2020, 0.34).
narrative_ontology:measurement_basis(gold_tr_t2020, observed).
narrative_ontology:measurement(gold_tr_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 2024, 0.33).
narrative_ontology:measurement_basis(gold_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1971, 0.46).
narrative_ontology:measurement_basis(gold_be_t1971, observed).
narrative_ontology:measurement(gold_be_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement_basis(gold_be_t1980, observed).
narrative_ontology:measurement(gold_be_t1990, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(gold_be_t1990, observed).
narrative_ontology:measurement(gold_be_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2000, 0.56).
narrative_ontology:measurement_basis(gold_be_t2000, observed).
narrative_ontology:measurement(gold_be_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement_basis(gold_be_t2008, observed).
narrative_ontology:measurement(gold_be_t2015, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement_basis(gold_be_t2015, observed).
narrative_ontology:measurement(gold_be_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement_basis(gold_be_t2020, observed).
narrative_ontology:measurement(gold_be_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 2024, 0.74).
narrative_ontology:measurement_basis(gold_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t1971, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1971, 0.66).
narrative_ontology:measurement_basis(gold_su_t1971, observed).
narrative_ontology:measurement(gold_su_t1980, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement_basis(gold_su_t1980, observed).
narrative_ontology:measurement(gold_su_t1990, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement_basis(gold_su_t1990, observed).
narrative_ontology:measurement(gold_su_t2000, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement_basis(gold_su_t2000, observed).
narrative_ontology:measurement(gold_su_t2008, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2008, 0.5).
narrative_ontology:measurement_basis(gold_su_t2008, observed).
narrative_ontology:measurement(gold_su_t2015, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(gold_su_t2015, observed).
narrative_ontology:measurement(gold_su_t2020, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement_basis(gold_su_t2020, observed).
narrative_ontology:measurement(gold_su_t2024, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 2024, 0.57).
narrative_ontology:measurement_basis(gold_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, resource_allocation).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the end of the gold standard' covers at least three structurally distinct claims, decomposed per the epsilon-invariance principle into three linked stories sharing kernel gold_fiat_transition_mechanism. This file (automatic_constraint_reading) authors the material-to-institutional substitution of the money-creation limit, with high epsilon borne by nominal claim-holders and collected by the issuing complex. The creditor_discipline_reading authors the same history as veto-power removal and geopolitical realignment; the composite_overdetermination_reading authors it as multi-cause convergence with no single causal node. Each story carries its own epsilon, beneficiaries, victims, and classification; the upstream/downstream citation pattern runs from this reading's substitution claim into the sibling framings, which incorporate or deny it as a component.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
