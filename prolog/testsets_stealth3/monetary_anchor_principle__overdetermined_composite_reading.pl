% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Bretton Woods Dollar-Gold Anchor Discipline, Terminal Phase (Overdetermined Composite Reading)
 *   domain: economic/monetary/political
 *
 * SUMMARY:
 *   Between 1958 and 1971 the Bretton Woods anchor — fixed parities against a
 *   dollar officially convertible into gold at thirty-five dollars per ounce
 *   for monetary authorities — operated in terminal condition while remaining
 *   formally intact. This story instantiates the
 *   overdetermined_composite_reading of the monetary_anchor_principle kernel:
 *   the anchor's collapse was produced by the joint action of four
 *   independent streams — the Triffin liquidity dilemma inherent in supplying
 *   world reserves through a national currency, the fiscal expansion of the
 *   Vietnam War and Great Society financed in part through dollar issuance, a
 *   governing consensus that subordinated price stability to full-employment
 *   management, and technologically enabled capital mobility (Eurodollar
 *   markets, telex-speed arbitrage) that made parity defense progressively
 *   more expensive. By this reading no single-stream remedy remained
 *   available by the late 1960s, which is why epsilon is authored high: the
 *   arrangement's extraction was not reversible without addressing every
 *   stream at once. EPSILON REFERENT: the standing arrangement is the
 *   dollar-gold anchor discipline itself, in its 1958-1971 operation,
 *   assessed by this reading's own lights — not the floating regime this
 *   reading takes the collapse to have produced, and not the decision event
 *   of August 1971 (that is the punctuated_swap sibling's referent, authored
 *   separately). CLAIM/METRIC INDEPENDENCE: the claimed_type is tangled_rope
 *   — the anchor genuinely solved the interwar competitive-devaluation
 *   problem for trading economies while simultaneously transferring
 *   seigniorage to the United States fiscal account under active enforcement
 *   — and the metrics are authored independently from that claim, describing
 *   heavy and rising extraction, a control apparatus that ratcheted upward,
 *   and theater crossing one-half after the Gold Pool's collapse. FAMILY
 *   DECOMPOSITION: this is one of three linked stories; the siblings
 *   (punctuated_swap_reading, triffin_inevitability_reading) hold different
 *   epsilon referents and different victim structures by construction, and
 *   the disagreement among them is located in the unit of causal analysis —
 *   conjunctural streams versus a discrete decision versus a single dilemma's
 *   logic.
 *
 * KEY AGENTS:
 *   - - us_fiscal_authorities: Agenda-setting collector (institutional/arbitrage) — administers the anchor, finances deficits through it, holds the unilateral exit that ends it
 *   - - foreign_central_banks: Enforced absorbers (organized/trapped) — hold the reserves, staff the defense, cannot convert without destroying their own holdings
 *   - - surplus_creditor_governments: Pressured dual-positioners (powerful/constrained) — absorb unwanted dollars and imported inflation while quietly collecting the undervaluation subsidy
 *   - - deficit_peg_economies: Forced adjusters (moderate/constrained) — bear deflationary stop-go cycles the reserve issuer postpones for itself
 *   - - internationally_oriented_exporters: Coordination collectors (organized/constrained) — earn in stable cross-rates, carry little of the fiscal burden
 *   - - private_gold_market_participants: Excluded revealers (moderate/constrained) — their two-tier price exposes the gap between declared and market value
 *   - - float_advocacy_economists: Excluded dissent (moderate/constrained) — the floating-rate case kept outside official councils until after the fact
 *   - - bis_annual_report_analysts: Analytical observer (moderate/analytical) — measures the widening gap, persuades no one with jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.74).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.65).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Bretton Woods Dollar-Gold Anchor Discipline, Terminal Phase (Overdetermined Composite Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "economic/monetary/political").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b').
narrative_ontology:cs_kernel_codification('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', formalized).
narrative_ontology:cs_authority_grounding('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', practice).
narrative_ontology:cs_interpretation_layer_present('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b').
narrative_ontology:cs_reading_relation('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', foundational, collapse_requires_conjunct_structural_streams).
narrative_ontology:cs_axiom_status(collapse_requires_conjunct_structural_streams, holdable).
narrative_ontology:cs_axiom_grounding('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', collapse_requires_conjunct_structural_streams, empirically_contingent).
narrative_ontology:cs_axiom('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', secondary, late_1960s_irreversibility_thesis).
narrative_ontology:cs_axiom_status(late_1960s_irreversibility_thesis, holdable).
narrative_ontology:cs_axiom_grounding('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', late_1960s_irreversibility_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', bretton_woods_par_value_architecture).
narrative_ontology:cs_drift_state('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', post_gold_pool_two_tier_regime, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('8b50fa6c-b8d7-4db6-bbde-4f44ad56d87b', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, internationally_oriented_exporters).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, surplus_creditor_governments).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, deficit_peg_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, surplus_creditor_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the arrangement and draws on it at once: the Treasury defends the thirty-five-dollar gold parity while the Federal Reserve supplies the swap lines and open-market operations that hold dollar parities, and the same fiscal account finances Southeast Asian war spending and domestic programs by issuing dollar liabilities that foreign monetary authorities must absorb as reserves. Each year brings the same choice — deflate the domestic economy to honor convertibility, or let gold drain — and each year the drain is chosen. Holds the unique exit: as issuer of the reserve asset it can terminate the arrangement unilaterally, which it does in August 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authorities, beneficiary).

% Hold the bulk of world dollar reserves earned from trade surpluses. Every conversion request pressures the parity; collective conversion would destroy the value of everyone's holdings at once, so each institution holds and absorbs creeping depreciation — a prisoner's dilemma among themselves. They staff and fund the London Gold Pool to defend the official price, and shut it down in March 1968 when the drain outruns their bullion. Individually too weak to renegotiate terms with the issuer; collectively unable to solve their own coordination problem.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, foreign_central_banks, beneficiary).

% West Germany and Switzerland: run persistent trade surpluses, accumulate unwanted dollar balances, import the issuer's inflation through the pegged rate, and face continuous American pressure to revalue or accommodate. Revaluation would tax their export industries — the undervalued parity operates as a quiet subsidy they collect — so they resist, threaten unilateral floats, negotiate in Basel corridors, and absorb the inflow while demanding the issuer put its own fiscal house in order.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, surplus_creditor_governments, payer,
    powerful, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, surplus_creditor_governments, beneficiary).

% The United Kingdom and comparable chronic-deficit peggers: defend overvalued parities with stop-go austerity, borrow through Fund stand-bys and Basle facilities, and devalue only under duress, as sterling did in November 1967. The adjustment the system demands lands on their wages and employment, while the reserve issuer postpones the same adjustment for itself year after year.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, deficit_peg_economies, payer,
    moderate, generational, constrained, regional).

% Trading firms and multinational manufacturers across Europe and Japan: invoice and plan in stable cross-rates, invest long-horizon capacity without hedging costs, and press their ministries to preserve the pegged grid. They collect the arrangement's coordination dividend directly and bear almost none of its fiscal burden.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, internationally_oriented_exporters, beneficiary,
    organized, biographical, constrained, continental).

% Dealers, speculators, and hoarders with no access to the official window: they can transact only in the free market, where the price decoupled above thirty-five dollars after March 1968. Their continuous bidding publishes the gap between the declared parity and gold's market valuation — information the official architecture declines to recognize.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, private_gold_market_participants, excluded,
    moderate, immediate, constrained, global).

% Academic monetarists, publishing the case for floating rates from the early 1950s: parities are unnecessary and destabilizing, adjustment should run through prices not reserves. They sit outside the treasury-central bank consensus that treats the pegged grid as the only respectable arrangement, and their proposals enter official discussion only after the window closes.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, float_advocacy_economists, excluded,
    moderate, biographical, constrained, global).

% Economists at the Basel-based Bank for International Settlements: measure gold flows, reserve composition, and Eurodollar growth in annual reports, diagnosing the widening distance between parity declarations and market reality. They see the whole structure earliest and hold no jurisdiction over any part of it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, bis_annual_report_analysts, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authorities).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common nominal anchor for world money: fixed parities against a gold-convertible dollar eliminate the competitive devaluations and exchange uncertainty of the interwar years and give every trading economy a single reference price level, with the Fund financing temporary disequilibria.
% TRANSFER_FUNCTION: Moves real resources to the United States fiscal account: foreign monetary authorities are compelled to absorb newly issued dollar liabilities as reserves, financing military expenditure abroad and domestic programs without contemporaneous taxation; symmetrically, it moves deflationary adjustment onto deficit pegging economies while the reserve issuer defers its own.
% ABSENT_VOICES: Floating-rate economists sat outside the treasury-central bank consensus that defined the admissible policy space; private gold holders were barred from the official window whose price they disproved daily; wage earners in creditor economies absorbed imported inflation with no seat in Fund quota-weighted governance; and deficit-country publics endured stop-go austerity negotiated over their heads between treasuries, central banks, and the Fund.
% DISAPPEARANCE_RATIONALE: Within twenty-six months of the window closing, the pegged grid dissolved into generalized floating, inflation across industrial economies roughly doubled through the following decade, reserve portfolios diversified away from the dollar, and oil producers repriced crude steeply against the depreciating reserve asset — trade invoicing, reserve management, and energy contracts all reorganized around the anchor's absence.
% FOUNDING_PROBLEM: Reconstructing international payments after the interwar breakdown: the 1930s delivered sequential competitive devaluations, discriminatory blocs, and deep depression; the 1944 designers sought stable but adjustable parities, gold anchoring the dollar, and a Fund to finance temporary deficits so that adjustment need not always mean deflation.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the negotiating record itself (Keynes's Clearing Union drafts, White's stabilization-fund blueprints) states the interwar-chaos objective explicitly; economic historians spanning otherwise opposed methodological schools attest both the founding problem and its persistence in mutated form; none of these sources collects anything from the anchor's operation.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__overdetermined_composite_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high and rising (0.42 to 0.74 across the interval) because the anchor's operation progressively decoupled US liability issuance from any gold backing — foreign monetary authorities were compelled to hold depreciating paper while the issuing state financed war and welfare without contemporaneous taxation. Suppression (0.65 at end) tracks the enforcement machinery the arrangement required once voluntary cooperation stopped sufficing: the Interest Equalization Tax of 1963, the Voluntary Credit Restraint Program of 1965, expanded Fed swap networks, Basle facility lending, and sustained diplomatic pressure on surplus countries not to convert. Per the framework's division of labor, suppression is authored as a raw structural property and is NOT scaled — only extraction is scaled, by directionality and spatial scope in the engine's computation; the anchor's global scope makes verification of backing harder and feeds the engine's scope amplifier on the extraction side. Theater_ratio crosses 0.5 exactly at the Gold Pool's March 1968 collapse, after which the official price survived only in the official channel while the private market priced gold far above parity — declaratory maintenance of a defunct discipline. Accessibility_collapse is authored 0.60: the floating-rate alternative was fully articulated (Friedman from 1953) and a partial substitute existed (SDRs from 1969), so alternatives never vanished, but the official consensus excluded them from consideration inside policy councils until the end. Resistance is authored 0.70 — French public attacks, German conversion threats and near-unilateral floats, the sterling crisis of 1967, successive speculative runs, and academic dissent — because a construct defended this expensively is by definition contested. COALITION CHECK: the principal victim class did coalize (the London Gold Pool was precisely a victim cartel) and the coalition failed through the classic free-rider collapse — each member's cheapest move was to let the others burn bullion first — which is why organized victims could not renegotiate terms and the extraction persisted. TEMPORAL SHAPE: the three series are monotonic rather than cyclical — an enforcement ratchet, not an oscillation — so no intermittent-reinforcement mechanism is implicated; all three metrics are authored on one shared six-point grid (1958, 1960, 1963, 1965, 1968, 1971 mapped to 0, 2, 5, 7, 10, 13), and each endpoint equals its base_properties scalar. The suppression_requirement series is included deliberately because the story's narrative specifically traces enforcement-capacity buildup, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the engine owns that computation. From the us_fiscal_authorities seat, the anchor appears as an instrument it administers and draws on — a constraint that bound everyone else while its administrator postponed adjustment for thirteen years, experienced as coordination it built. From the foreign_central_banks seat, the identical structure operates as enforced absorption: obliged to hold the issuer's paper, unable to convert without self-harm, staffing a defense fund that consumed their own bullion. Surplus_creditor_governments occupy a genuinely ambivalent position — taxed by imported inflation yet subsidized by undervalued parities — which is why their seat should land nearer the middle than their victim-listing alone would suggest. Deficit_peg_economies experience the sharpest asymmetry: they performed the adjustment the system demanded, promptly and painfully, while the issuer deferred its own indefinitely. The exporter seat sees almost pure coordination — stable cross-rates, no hedging cost — and carries essentially none of the extraction, which is exactly the signature of a hybrid rather than a pure-extraction arrangement. The excluded seats (private gold holders, floating-rate economists) perceive the declaratory layer as fiction well before the officials do.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation: us_fiscal_authorities sits at the beneficiary pole (collects the seigniorage, controls the rules — d near zero, extraction damped or inverted toward subsidy for it), internationally_oriented_exporters likewise near the beneficiary end with mobile-enough commercial positions. foreign_central_banks derive high d from victim status compounded by trapped exit — their reserves lose value if converted and bleed if held, the canonical no-good-exit position. deficit_peg_economies derive high d from victim status with constrained exit (devaluation possible but catastrophic and politically fraught, as November 1967 demonstrated). DIRECTIONALITY OVERRIDE: surplus_creditor_governments are the one seat where the structural derivation fails — listed among victims, a derivation from victim-plus-powerful would push them toward the full-target end, but their actual relationship is materially tempered by the undervaluation subsidy their export industries collected and by their genuine threat power (Germany could and nearly did float unilaterally in 1969-1971). Because the override surface keys on the power atom, the story assigns surplus_creditor_governments the unique 'powerful' atom among the stakeholders and overrides that atom to d=0.55, capturing the ambivalent middle position; no other stakeholder shares the atom, so the correction lands only where intended. The derivation is left untouched for every other seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The anchor exhibits a clean two-phase lifecycle that the classification apparatus is built to keep distinct. Through 1968 the arrangement retained a live mandate: parities held, trade expanded under stable rates, and the extraction, though growing, rode on a functioning coordination machine — a hybrid in the full sense, with the enforcement burden rising as the extraction rose. After the Gold Pool collapsed, the mandate was dead — the discipline the anchor existed to impose was no longer imposed on its administrator, and the official parity persisted as declaration — yet the arrangement was not dismantled for another forty months; it was maintained performatively, which is why mandatrophy is declared resolved and why theater_ratio crosses one-half at interval end. The tangled_rope claim is authored for the interval as a whole and is deliberately NOT reconciled to the terminal-phase theater signal: the engine reads the temporal series and may locate the terminal drift toward theatrical inertia on its own, which is precisely the measurement the corpus exists to take. The classification guards against two symmetrical mislabels: calling the whole thing pure coordination (which would erase thirteen years of compounding seigniorage transfer and the enforced absorption of reserve holders), and calling it pure extraction (which would erase the real interwar-problem solution that exporters and trading economies collected throughout, and which explains why so many participants defended the arrangement even while being extracted from). The R5 interview reinforces the boundary: the founding problem (interwar chaos) was real and remains live in mutated form, but the specific mandate — gold-backed discipline binding the issuer — was dead years before the body was buried.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'This story is one reading of the monetary_anchor_principle kernel, namely the overdetermined_composite_reading. How would instantiating a sibling reading (punctuated_swap_reading or triffin_inevitability_reading) change the epsilon referent, the victim structure, and the resulting classification?',
    'Author the two sibling stories as separate constraint files and compare computed types: the punctuated_swap reading takes the August 1971 decision event itself as the standing arrangement (a discrete institutional act, plausibly carrying a different beneficiary/victim split centered on the choice-makers); the triffin reading isolates the reserve-currency dilemma as the single operative stream, concentrating epsilon attribution on the liquidity-supply obligation.',
    'Each reading instantiates a different constraint with a different epsilon over a different referent; cross-reading comparisons of epsilon or type are invalid unless routed through the family network edges recorded in network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Committer-frame routing: kernel membership, this instantiation, and the structural deltas sibling readings would produce.').

omega_variable(
    overdetermination_counterfactual_identifiability,
    'Can historical counterfactual analysis establish the conjunct-necessity claim — that removing any single causal stream (Vietnam-era deficits, the Keynesian full-employment consensus, technological capital mobility, or the Triffin liquidity logic alone) would NOT have preserved the anchor beyond 1971?',
    'Systematic counterfactual historiography: model anchor survival under each single-stream intervention (no Southeast Asian escalation with the Gold Pool intact; early binding capital controls choking Eurodollar arbitrage; a credible US adjustment package in 1969-1970) and code each for survival probability.',
    'If any single-stream intervention credibly saves the arrangement past 1971, the composite reading loses its distinguishing axiom, extraction attribution migrates toward whichever stream proves decisive, and the reading converges toward the triffin monocause sibling with a correspondingly narrower victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermination_counterfactual_identifiability, empirical, 'Whether the overdetermination thesis is empirically identifiable or permanently underdetermined by the historical record.').

omega_variable(
    seigniorage_capture_seat,
    'Does the extraction demonstrably accrue to the named seat (us_fiscal_authorities), or does it dissipate diffusely across domestic constituencies — defense contractors, welfare recipients, general taxpayers relieved of contemporaneous taxation — with the Treasury acting as mere conduit?',
    'Fiscal-incidence tracing of 1965-1971 deficit finance: follow seigniorage-funded outlays to final recipients and assess incidence of the counterfactual in which convertibility was honored (who would have borne the taxes).',
    'If diffuse, the receipt surface should be re-authored as diffuse and the constraint drifts toward cost-asymmetry-without-capturer rather than captured extraction; if concentrated at the fiscal seat, the named-seat capture verdict strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seigniorage_capture_seat, empirical, 'Where the gains of the anchor''s erosion actually land.').

omega_variable(
    terminal_theater_vs_residual_function,
    'After the March 1968 collapse of the London Gold Pool, was parity maintenance pure performance, or did the official thirty-five-dollar window retain a real clearing function among central banks?',
    'Ledger analysis of official-settlements gold transactions 1968-1971 against the private two-tier market price and the reserve-stock adjustments they accommodated; negligible official volumes relative to the adjustments they ratified indicate ceremony.',
    'A pure-performance verdict pushes the terminal-phase classification toward theatrical inertia (piton-shaped); a residual-function verdict keeps the hybrid coordination-plus-extraction profile intact through the interval end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terminal_theater_vs_residual_function, conceptual, 'Whether post-1968 parity declarations were functional or ceremonial.').

omega_variable(
    inevitability_claim_scope,
    'Is inevitability-by-the-late-1960s a property of the arrangement itself, or an artifact of this reading''s retrospective framing — would contemporaries holding the same information have judged a defense package feasible?',
    'Code contemporaneous internal deliberations (Treasury task-force records, Federal Reserve minutes 1969-1971, the Volcker stabilization plan of June 1971) for perceived feasibility of defense packages, separating structural impossibility from political rejection.',
    'If credible defense packages existed and were rejected for electoral or administrative reasons, inevitability softens into contingency, this reading''s extraction profile drops toward a chosen-policy shape, and the punctuated_swap sibling gains ground as the better description of the transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_claim_scope, conceptual, 'Whether the collapse was structurally inevitable or contingently chosen.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(mone_tr_t2, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(mone_tr_t7, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 7, 0.28).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 10, 0.46).
narrative_ontology:measurement(mone_tr_t13, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 13, 0.52).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mone_be_t2, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(mone_be_t7, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 7, 0.61).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 10, 0.69).
narrative_ontology:measurement(mone_be_t13, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 13, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(mone_su_t2, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 2, 0.36).
narrative_ontology:measurement(mone_su_t5, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(mone_su_t7, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 7, 0.56).
narrative_ontology:measurement(mone_su_t10, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(mone_su_t13, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 13, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% Constraint family: monetary_anchor_principle decomposes into three reading-stories because the natural-language label 'end of Bretton Woods' covers structurally distinct claims with distinct epsilon referents (epsilon-invariance decomposition). This story (overdetermined_composite_reading) authors epsilon for the whole anchor arrangement under multi-stream erosion; triffin_inevitability_reading authors epsilon for the arrangement seen through a single dilemma's logic (upstream in rhetorical dependence — composite arguments cite Triffin as one stream, so the monocause story functions as a cited component); punctuated_swap_reading authors epsilon for the transition-as-decision-event (downstream, taking the composite process as settled background and claiming only the act). Each member links the others via network.affects_constraints; cross-reading metric comparisons are invalid except along these edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
