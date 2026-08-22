% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods Substrate as Constraint on Government Intervention (Free Capital Markets Reading)
 *   domain: economic/political/monetary_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Bretton Woods kernel: the
 *   substrate (Articles of Agreement, IMF machinery, dollar anchor, and the
 *   treaty-and-code lattice grown around them) as a commitment device that
 *   binds governments against intervening in cross-border capital flows, so
 *   that free capital markets can operate. Under this reading, capital
 *   controls are violations rather than tools, international finance is the
 *   protected class, and national policy autonomy is what the arrangement
 *   takes as its price. Per the epsilon-invariance rule, the sibling readings
 *   (keynesian_embedded_liberalism, sovereignty_defense) are separate
 *   constraints in separate files with inverted victim structures; nothing
 *   about them is averaged into this file. The claim and the metrics are
 *   independent authored facts: I claim tangled_rope because I judge the
 *   arrangement to possess both a genuine coordination function (settlement,
 *   liquidity pooling, risk pricing) and enforced asymmetric collection
 *   (autonomy and crisis costs moved from polities to mobile claimants); the
 *   metrics describe the arrangement's actual operation as the record shows
 *   it.
 *
 * KEY AGENTS:
 *   - global_financial_institutions: primary beneficiary (powerful/arbitrage) — collects spreads, fees, and bailout-backed yields; enforces discipline by repricing sovereign risk
 *   - portfolio_investors: secondary beneficiary (powerful/arbitrage) — mobile claimants guaranteed first exit
 *   - imf_and_standard_setters: agenda-setter (institutional/constrained) — administers articles, surveillance, and conditionality
 *   - us_treasury_and_federal_reserve: dual-positioned issuer (institutional/constrained) — collects reserve-currency privilege, forfeits exchange-rate autonomy
 *   - small_open_economies: primary target (moderate/trapped) — macro toolkit contractually and reputationally sealed
 *   - developing_economy_populations: diffuse target (powerless/trapped) — absorb austerity and sudden-stop unemployment
 *   - advanced_economy_electorates: target with offsetting gains (organized/constrained) — cheap credit and asset growth against pre-committed fiscal policy
 *   - heterodox_capital_management_advocates: excluded voice (organized/constrained) — heard after crises, ignored between them
 *   - international_monetary_economists: analytical observer (analytical/analytical) — documents the gap between 1944 text and operative doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.66).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.72).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.66).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods Substrate as Constraint on Government Intervention (Free Capital Markets Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "economic/political/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '87a8bed8-62fb-47b1-b7ec-d15104236952').
narrative_ontology:cs_kernel_codification('87a8bed8-62fb-47b1-b7ec-d15104236952', formalized).
narrative_ontology:cs_authority_grounding('87a8bed8-62fb-47b1-b7ec-d15104236952', expertise).
narrative_ontology:cs_interpretation_layer_present('87a8bed8-62fb-47b1-b7ec-d15104236952').
narrative_ontology:cs_reading_relation('87a8bed8-62fb-47b1-b7ec-d15104236952', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, forecloses).
narrative_ontology:cs_reading_relation('87a8bed8-62fb-47b1-b7ec-d15104236952', bretton_woods_treaty_substrate__sovereignty_defense, forecloses).
narrative_ontology:cs_axiom('87a8bed8-62fb-47b1-b7ec-d15104236952', foundational, free_capital_flows_maximize_welfare).
narrative_ontology:cs_axiom_status(free_capital_flows_maximize_welfare, holdable).
narrative_ontology:cs_axiom_grounding('87a8bed8-62fb-47b1-b7ec-d15104236952', free_capital_flows_maximize_welfare, empirically_contingent).
narrative_ontology:cs_axiom('87a8bed8-62fb-47b1-b7ec-d15104236952', foundational, capital_property_rights_precede_state_discretion).
narrative_ontology:cs_axiom_status(capital_property_rights_precede_state_discretion, holdable).
narrative_ontology:cs_axiom_grounding('87a8bed8-62fb-47b1-b7ec-d15104236952', capital_property_rights_precede_state_discretion, deontological).
narrative_ontology:cs_reference_frame('87a8bed8-62fb-47b1-b7ec-d15104236952', free_capital_convertibility_norm).
narrative_ontology:cs_drift_state('87a8bed8-62fb-47b1-b7ec-d15104236952', contemporary_fragmentation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('87a8bed8-62fb-47b1-b7ec-d15104236952', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, global_financial_institutions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, portfolio_investors).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, small_open_economies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economy_populations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, advanced_economy_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, advanced_economy_electorates).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, mundell_fleming_trilemma_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__neoliberal_convertibility, efficient_capital_market_allocation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cross-border banks, asset managers, and currency dealers that intermediate the world's savings. They collect spreads, fees, and yields on balances that can be relocated between jurisdictions within hours, and they enforce repayment norms by repricing sovereign debt and withdrawing funding when a government reaches for tools the framework forbids. Their exit is the fastest in the system: capital moves before legislation does.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, global_financial_institutions, beneficiary,
    powerful, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, global_financial_institutions, agenda_setter).

% Pension funds, mutual funds, and hedge funds that allocate savings across borders. They depend on unrestricted entry and exit, protection from forced restructuring, and deep liquid markets for selling at will. When a debtor country tries to stem outflows, their ability to leave first is precisely what the open-account rules guarantee.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, portfolio_investors, beneficiary,
    powerful, immediate, arbitrage, global).

% The IMF, BIS, OECD, and G7 treasury apparatus that administers the Articles of Agreement, conducts surveillance, attaches conditions to crisis lending, and maintains the liberalization codes. The same bodies pushed capital-account opening in the 1990s and, since 2012, tolerate narrowly framed capital flow management. They are financed by quotas dominated by advanced economies and cannot walk away from the system they staff without dissolving their own mandate.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_and_standard_setters, agenda_setter,
    institutional, generational, constrained, global).

% Issues the system's reserve asset. Global demand for dollars and Treasuries lets the United States borrow cheaply and run deficits others finance, and gives its sanctions leverage over any bank touching the dollar. At the same time, the issuer forfeits the exchange-rate tool, imports other regions' crises through dollar funding cycles, and is locked in by the privileges it would surrender by stepping off the anchor.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, us_treasury_and_federal_reserve, payer).

% Governments of trade-dependent states whose macroeconomic toolkit — capital controls, countercyclical monetary policy, managed exchange rates — is penalized by treaty commitments, credit ratings, and funding access. Deploying a retained tool invites capital flight and conditionality; leaving the dollar-centered system entirely would sever trade finance and invite isolation. They keep the tools in a glass case they may not open.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, small_open_economies, payer,
    moderate, generational, trapped, national).

% Absorb austerity, unemployment, and inflation when sudden stops hit, while creditor claims are honored first. They have no vote in quota-weighted governance, and emigration is bounded by destination-country borders. Crisis costs land on their wages and services; the upside of open accounts accrues elsewhere.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_economy_populations, payer,
    powerless, biographical, trapped, national).

% Receive cheaper credit, imported goods, and rising pension-asset values from open capital markets, and pay in the form of fiscal choices pre-committed to market confidence, regional contagion losses, and bailouts directed at creditors. They cannot exit their own polities, and their voting power operates only on the slow margin of electoral cycles.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, advanced_economy_electorates, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, advanced_economy_electorates, beneficiary).

% Post-Keynesian economists, UNCTAD analysts, and policymakers from episodes such as Malaysia in 1998 and Iceland after 2008 who argue for capital management techniques as ordinary tools. They publish, advise, and testify, but sit outside the Basel, G7, and quota-weighted rooms where the rules are written; their proposals gain a hearing mainly in the aftermath of crises and recede as memory fades.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, heterodox_capital_management_advocates, excluded,
    organized, generational, constrained, global).

% Researchers who trace the treaty's drafting history, the liberalization record, and the divergence between the 1944 text and later doctrine. They hold no enforcement stake and can compare regimes across cases that participants must live inside.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, global_financial_institutions).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common dollar-centered framework for cross-border saving and payment: predictable conversion, deep settlement markets, pooled crisis liquidity, and a single unit in which most trade and debt are invoiced. Fragmented national capital markets and currency risk for international investors and traders are solved once, centrally, instead of bilaterally.
% TRANSFER_FUNCTION: Moves policy discretion from national governments to holders of mobile capital; moves global savings toward whichever jurisdiction offers return or safe haven; and, in crises, moves adjustment costs onto deficit-country labor and public budgets while honoring creditor claims ahead of them.
% ABSENT_VOICES: Capital-management advocates, quota-underrepresented debtor governments, and the populations bearing crisis adjustment are not in the Basel, G7, or quota-weighted rooms where liberalization rules are set. They enter chiefly as petitioners after crises, on terms the creditors' institutions draft, and their dissent surfaces in communiqué footnotes rather than in the operating rules.
% DISAPPEARANCE_RATIONALE: If the intervention ban and its enforcement vanished overnight, capital controls would proliferate within months, cross-border portfolios would fragment along currency blocs, dollar funding markets would reprice violently, and the invoicing and settlement plumbing built around free convertibility would need explicit political reconstruction. The global financial economy is organized around this constraint and would reorganize without it.
% FOUNDING_PROBLEM: Designed in 1944 to end the interwar spiral of competitive devaluation, retaliatory trade policy, and destabilizing short-term capital flows that had broken governments and fed fascist mobilization.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians outside the arrangement's benefiting parties (Eichengreen, Helleiner, Bordo) attest the founding problem as interwar disorder — and the archival record shows the original text aimed to restrain speculative capital, with controls expressly permitted. No source outside the benefiting set attests that the treaty was founded to bind governments for capital's benefit; this reading's genealogy therefore rests on later doctrinal accretion, and that absence is itself the signal.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.66 at interval end: the arrangement genuinely pools liquidity and prices risk, but its crisis record shows gains privatized and losses socialized, and its standing rules seal off the policy instruments debtor states would otherwise hold. Suppression is higher (0.72) because persistence depends on active machinery — treaty chapters, OECD and EU commitments, IMF conditionality, ratings and funding pressure — not on participant preference; suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope, unlike extractiveness. Theater ratio (0.40) tracks the widening share of activity that performs efficiency and rule-of-law rhetoric while distributing crisis rents. Accessibility collapse is moderate (0.50): exits exist and have been used (Malaysia 1998, Iceland 2008, Brazil's inflow tax), but each use carries escalating reputational and funding cost, so alternatives narrow without vanishing. Resistance (0.58) is persistent and recurring rather than systemic. The measurement series runs on one shared seven-point grid (1944–2024) so every metric is authored at every examined time point; the trajectories show accumulation to a 1997 peak, a post-2008 doctrinal retreat, and a renewed hardening of enforcement machinery (sanctions architecture, secondary enforcement) even as official doctrine softened — suppression_requirement rising while extractiveness plateaus is the signature of enforcement capacity maturing independently of extraction level.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/agenda-setter seats compute differently from the same structure. From the small_open_economies and population seats, the arrangement is a sealed toolbox: real instruments behind glass, enforceable penalties for reaching. From the finance seats, it is indispensable infrastructure they fund and police. The IMF seat straddles: it administers the enforcement while supplying the counter-cyclical liquidity that makes the enforcement survivable, which is why its directionality is overridden rather than derived. The US seat is genuinely dual — largest collector of the system's seigniorage and largest bearer of its lock-in. Identity-lock operates at two seats: the US institutional identity as system guarantor makes questioning the anchor unthinkable from inside the building, and advanced-economy electorates are citizenship-locked into polities whose fiscal space is pre-committed. If the US guarantor identity broke, the anchor's politics would change faster than its economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure drives the derivation: global_financial_institutions and portfolio_investors are declared beneficiaries with arbitrage exit, placing them near the full-beneficiary end; small_open_economies (trapped), developing_economy_populations (trapped), and advanced_economy_electorates (constrained) are declared victims, placing them near the full-target end, with trapped exit sitting farther toward the target pole than constrained. The IMF seat holds no beneficiary or victim declaration, so its canonical fallback would miss its dual posture as enforcer and subsidizer; a single directionality override at the institutional power atom (d = 0.32) encodes mild net collection. The override is coarse — it also touches the US institutional seat — but the US dual role lands near the same value, so the approximation is honest. Coalition potential among the payer seats is real but unrealized: G77 blocs, the Chiang Mai Multilateralization, and BRICS contingency arrangements exist, yet none has yet substituted for dollar-system access at scale.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar chaos) was originally to be solved by instruments this reading reverses: the 1944 design restrained capital and protected policy space. The arrangement survived the death of that operating logic — fixed rates and gold ended in 1971–73 — by re-founding on liberalization doctrine rather than by atrophying. It is therefore not a piton: the function transformed rather than decayed, and the theater_ratio rise measures the growing gap between the efficiency narrative and the rent distribution, not the replacement of function by performance. The classification resists both mislabels: calling this a pure snare would erase the real coordination (settlement, liquidity pooling, risk pricing that no bilateral arrangement replicates); calling it a pure rope would erase the enforced asymmetry that names identifiable payers. Tangled rope is the structural truth the data supports, and the founding_problem_status 'contested' verdict records that the genealogy itself is one of the disputed objects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_of_bretton_woods_kernel,
    'This constraint is one reading (neoliberal_convertibility) of the kernel bretton_woods_treaty_substrate; what would the sibling readings change structurally?',
    'Compare against the sibling files: keynesian_embedded_liberalism inverts the victim set (international capital becomes the target, domestic policy space the protected good) and sovereignty_defense re-targets external discipline (IMF conditionality and market punishment become the constraint, national discretion the protected good). The disagreement is located in the binding target: government, capital, or external disciplinarian.',
    'Classification flips across readings: the same treaty substrate computes as payer-protecting under the Keynesian reading and as payer-binding under this one. Cross-reading comparison is valid only at the kernel level, never by merging the files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_of_bretton_woods_kernel, conceptual, 'Committer structure: one reading of a contested kernel, with the contest routed here rather than into the constraint body.').

omega_variable(
    original_text_vs_operative_reading,
    'Does the 1944 treaty text — which expressly permits capital controls under Article VI — support this reading''s claim that the substrate binds governments rather than capital?',
    'Textual and legal-historical analysis separating the founding instrument from later accretion: Eurodollar market growth, OECD liberalization codes, EU treaty capital chapters, bilateral investment treaties, and the IMF''s 1990s capital-account push.',
    'If the binding-on-government content is post-hoc accretion rather than treaty substance, this reading''s epsilon attaches to an evolved regime rather than the founding instrument, weakening its lineage claim and shifting the genealogy from founding intent to doctrinal capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_text_vs_operative_reading, empirical, 'Whether the neoliberal reading is grounded in the treaty''s actual text or in later doctrinal layers.').

omega_variable(
    efficiency_extraction_boundary,
    'How much of the measured collection is the irreducible price of coordinating global capital markets, and how much is separable rent captured by mobile claimants?',
    'Controlled comparison of liberalized versus managed regimes at similar development stages: Malaysia 1998 against regional neighbors, Chile''s encaje period, China''s sequencing, and the IMF''s own capital-flow-management evaluations.',
    'If most of the measured burden is coordination cost, the classification trends toward rope; if the burden is separable from the coordination function, it trends toward snare and supports unbundling reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficiency_extraction_boundary, empirical, 'Separability of the coordination function from the asymmetric collection riding on it.').

omega_variable(
    crisis_socialization_contingency,
    'Is the pattern of privatized gains and socialized losses intrinsic to the convertibility commitment, or contingent on the crisis-management doctrine of the 1990s and 2000s?',
    'Cross-case comparison of crisis resolution terms: Korea 1997, Argentina 2001, Greece 2010 versus Iceland 2008, where creditors took haircuts and controls were tolerated.',
    'If contingent, the asymmetry is doctrinal and reformable inside the reading; if intrinsic, the commitment device structurally transfers tail risk to debtor polities and the extraction estimate should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crisis_socialization_contingency, empirical, 'Whether creditor-priority crisis resolution is a necessary feature of the arrangement.').

omega_variable(
    weaponized_interdependence_drift,
    'Does the post-2022 weaponization of dollar infrastructure — sanctions, reserve freezes, secondary enforcement — alter the beneficiary structure by converting neutral coordination into alignment-conditioned access?',
    'Track reserve diversification, parallel settlement build-out, and hedging behavior by non-aligned states over the coming decade.',
    'If durable, the finance seats'' beneficiary position becomes conditional on geopolitical alignment, raising effective collection from non-aligned states and accelerating fragmentation of the coordination function itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(weaponized_interdependence_drift, empirical, 'Whether recent enforcement hardening changes who the arrangement serves.').

omega_variable(
    internalized_policy_taboo,
    'How much of the measured suppression is structural (treaty obligations, funding pressure) and how much internalized (policymaker conviction that intervention is illegitimate even where legal space remains)?',
    'Post-crisis policy trajectories: whether governments that regain space after crises actually use retained tools, or continue to self-restrain; comparative coding of finance-ministry doctrine before and after crisis episodes.',
    'If internalized, effective suppression exceeds the structural measure — the taboo travels with officials across administrations and survives formal legal room to maneuver; the omega separates the two mechanisms the scalar cannot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_policy_taboo, conceptual, 'Structural versus internalized components of the intervention taboo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.16).
narrative_ontology:measurement(bret_tr_t1986, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1986, 0.22).
narrative_ontology:measurement(bret_tr_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1997, 0.3).
narrative_ontology:measurement(bret_tr_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.22).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1958, 0.28).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.38).
narrative_ontology:measurement(bret_be_t1986, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1986, 0.55).
narrative_ontology:measurement(bret_be_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1997, 0.7).
narrative_ontology:measurement(bret_be_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2008, 0.62).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.15).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1958, 0.2).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.35).
narrative_ontology:measurement(bret_su_t1986, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1986, 0.5).
narrative_ontology:measurement(bret_su_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1997, 0.68).
narrative_ontology:measurement(bret_su_t2008, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bretton Woods' covers three structurally distinct claims about what the treaty substrate binds. This file instantiates the neoliberal_convertibility reading alone: the constraint binds government intervention, capital controls count as violations, and epsilon is authored for that arrangement only. The sibling files author inverted victim and beneficiary structures over the same substrate; per the epsilon-invariance decomposition rule they are linked here as a constraint family rather than merged, because measuring the substrate through different bindings yields different, stable epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
