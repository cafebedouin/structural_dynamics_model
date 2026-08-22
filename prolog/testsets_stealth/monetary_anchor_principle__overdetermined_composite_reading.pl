% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Bretton Woods Gold-Exchange Anchor (Overdetermined Composite Reading)
 *   domain: economic/political/monetary
 *
 * SUMMARY:
 *   This story instantiates the overdetermined_composite_reading of the
 *   monetary_anchor_principle kernel: the Bretton Woods gold-exchange anchor
 *   (1944-1971) is assessed as an arrangement that genuinely coordinated
 *   postwar reconstruction and trade - fixed-but-adjustable parities,
 *   current-account convertibility, and an expandable reserve asset - while
 *   simultaneously channeling seigniorage to the United States and adjustment
 *   burdens to everyone else. By this reading the anchor's collapse was
 *   neither a discrete choice nor a single-mechanism fate but the convergent
 *   product of four structural streams - the Triffin liquidity arithmetic, US
 *   fiscal expansion for war and welfare, the Keynesian full-employment
 *   consensus that subordinated price stability, and technologically driven
 *   capital mobility - such that by the late 1960s no feasible policy mix
 *   preserved convertibility. The sibling readings (punctuated_swap_reading,
 *   triffin_inevitability_reading) are separate constraint stories linked
 *   through network.affects_constraints; this file neither averages over them
 *   nor hedges across them. The claimed type and the authored metrics are
 *   independent facts: the type is claimed from the reading's structural
 *   lights, the metrics from the regime's recorded operation, and the engine
 *   computes per-seat classifications from the structural data without
 *   reference to the claim. KEY AGENTS (by structural relationship): -
 *   us_treasury_and_federal_reserve: Agenda setter and primary beneficiary
 *   (institutional/arbitrage) - issues the reserve asset, defends or suspends
 *   convertibility at discretion, collects seigniorage -
 *   export_oriented_surplus_economies: Compensated beneficiary
 *   (powerful/constrained) - grows behind undervalued parities while
 *   accumulating dollar claims of thinning gold backing -
 *   foreign_dollar_reserve_holders: Primary target (organized/constrained) -
 *   must hold the reserve asset to defend their own pegs and cannot redeem it
 *   wholesale - deficit_countries_under_adjustment: Target
 *   (moderate/constrained) - pays for parity defense in deflationary
 *   austerity and periodic devaluation -
 *   wage_earners_and_fixed_income_savers: Diffuse target (powerless/trapped)
 *   - bears the inflation tax as the anchor leaks; no legal exit into gold -
 *   gold_producers_under_price_cap: Priced-out supplier and excluded voice
 *   (organized/trapped) - sells into an official market pinned below clearing
 *   price - imf_management: Administering enforcer
 *   (institutional/identity_locked) - runs the parity grid and
 *   conditionality; constituted by the order it services -
 *   floating_rate_economists: Excluded analyst (moderate/constrained) - holds
 *   the outside alternative and no seat in the design conversation -
 *   monetary_history_analysts: Analytical observer (analytical/analytical) -
 *   reconstructs the ledger after the fact
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.74).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.68).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Bretton Woods Gold-Exchange Anchor (Overdetermined Composite Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "economic/political/monetary").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, '7545cdcf-361f-4247-a64d-d5fb78f9c770').
narrative_ontology:cs_kernel_codification('7545cdcf-361f-4247-a64d-d5fb78f9c770', fixed_text).
narrative_ontology:cs_authority_grounding('7545cdcf-361f-4247-a64d-d5fb78f9c770', extraction).
narrative_ontology:cs_interpretation_layer_present('7545cdcf-361f-4247-a64d-d5fb78f9c770').
narrative_ontology:cs_reading_relation('7545cdcf-361f-4247-a64d-d5fb78f9c770', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('7545cdcf-361f-4247-a64d-d5fb78f9c770', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_axiom('7545cdcf-361f-4247-a64d-d5fb78f9c770', foundational, multi_stream_overdetermination).
narrative_ontology:cs_axiom_status(multi_stream_overdetermination, holdable).
narrative_ontology:cs_axiom_grounding('7545cdcf-361f-4247-a64d-d5fb78f9c770', multi_stream_overdetermination, empirically_contingent).
narrative_ontology:cs_axiom('7545cdcf-361f-4247-a64d-d5fb78f9c770', secondary, rescue_window_closed_by_1968).
narrative_ontology:cs_axiom_status(rescue_window_closed_by_1968, holdable).
narrative_ontology:cs_axiom_grounding('7545cdcf-361f-4247-a64d-d5fb78f9c770', rescue_window_closed_by_1968, empirically_contingent).
narrative_ontology:cs_reference_frame('7545cdcf-361f-4247-a64d-d5fb78f9c770', managed_gold_exchange_equilibrium).
narrative_ontology:cs_drift_state('7545cdcf-361f-4247-a64d-d5fb78f9c770', post_gold_pool_collapse, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7545cdcf-361f-4247-a64d-d5fb78f9c770', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_and_federal_reserve).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, export_oriented_surplus_economies).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, foreign_dollar_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, deficit_countries_under_adjustment).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, wage_earners_and_fixed_income_savers).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, gold_producers_under_price_cap).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the dollar, pledges convertibility at thirty-five dollars per ounce for official holders, and decides when to defend that pledge and when to suspend it. Finances war and social-program deficits that are settled abroad in its own IOUs, collecting the margin between paper issued and metal pledged. Its exit is unilateral: it can close the gold window, revalue, or impose controls, and in August 1971 it does exactly that.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_and_federal_reserve, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_and_federal_reserve, beneficiary).

% Germany and Japan grow behind parities set below their post-recovery productivity, selling into a currency zone anchored by the dollar. They accumulate dollar reserves as the counterpart of their surpluses, come under repeated American pressure to revalue or absorb inflation, and watch the gold backing of their paper wealth thin year by year. Leaving means revaluation shocks to their export industries; staying means holding claims of doubtful redemption.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, export_oriented_surplus_economies, beneficiary,
    powerful, generational, constrained, global).

% Central banks and finance ministries outside the United States hold working balances in dollars because their own currencies are pegged to it. They cannot sell the balances without breaking the pegs they are defending and crushing their exporters, and they cannot redeem them wholesale because the gold stock does not exist to cover them; by 1971 their combined holdings exceed the US gold stock several times over.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, foreign_dollar_reserve_holders, payer,
    organized, biographical, constrained, global).

% Britain and other chronic deficit members defend their parities with stop-go austerity: credit squeezes, wage freezes, import surcharges, and negotiated support packages, followed when defense fails by devaluations that wipe out holders of their currency. The alternative to deflation is expulsion from the payments system and loss of reserve support.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, deficit_countries_under_adjustment, payer,
    moderate, biographical, constrained, regional).

% Hold wages and wealth denominated in national currencies whose purchasing power erodes as the anchor leaks; US consumer inflation roughly quintuples between the early 1960s and 1971. Private gold holding is illegal for Americans through the whole interval, opting out of the currency is impossible, and the loss arrives silently through prices.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, wage_earners_and_fixed_income_savers, payer,
    powerless, immediate, trapped, national).

% South African mines and other producers sell into an official market pinned at thirty-five dollars per ounce by the gold pool's sales while private demand bids above the pin. After March 1968 the pool closes and a two-tier market openly separates the official price from the market price; producers remain barred from the official channel's upside and have no other buyer of comparable scale.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, gold_producers_under_price_cap, payer,
    organized, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__overdetermined_composite_reading, gold_producers_under_price_cap, excluded).

% Administers the parity grid, brokers standby credits, and negotiates conditionality with deficit members within limits the US seat effectively sets. Its mandate, staffing, and professional self-conception are constituted by the Articles it services; it interprets members' obligations flexibly to keep them inside the system, and it has no existence apart from the order it administers.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, imf_management, agenda_setter,
    institutional, generational, identity_locked, global).

% Argue from universities and columns that parities should be set by markets and that the defense machinery wastes real resources. They publish freely but hold no seat in the treasuries and central banks running the system; their program is adopted, abruptly and without attribution, only after the system fails.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, floating_rate_economists, excluded,
    moderate, generational, constrained, national).

% Reconstruct reserve-cover ratios, meeting minutes, and decision records after the fact. They hold no position in the flows they study and can compare the regime's stated premises against its ledger outcomes across the whole interval.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__overdetermined_composite_reading, monetary_history_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__overdetermined_composite_reading, us_treasury_and_federal_reserve).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided fixed-but-adjustable exchange parities, current-account convertibility, and an expandable reserve asset (dollar claims redeemable in gold for official holders), so that postwar trade could rebuild without the interwar pattern of competitive devaluations and chronic international liquidity shortage.
% TRANSFER_FUNCTION: Moved seigniorage and adjustment costs: the United States issued dollar claims accepted for real goods and assets; deficit countries compressed demand and wages to defend parities; surplus countries exchanged real exports for accumulating dollar balances of declining gold backing; wage earners absorbed the resulting price drift.
% ABSENT_VOICES: Floating-rate economists argued from outside the managed-consensus policy circle and were excluded from design conversations until after the collapse; gold-price revisionists and producer governments were shut out of reserve-asset planning; surplus-country households absorbing imported inflation were never represented in parity decisions.
% DISAPPEARANCE_RATIONALE: Within eighteen months of the window closing, every major currency was floating, the Smithsonian realignment had failed, gold had more than doubled in price, and the inflation of the 1970s was underway; generalized floating, SDR issuance, and later petrodollar recycling replaced the parity grid - the trading and financial world reorganized around the anchor's absence.
% FOUNDING_PROBLEM: The interwar monetary breakdown: competitive devaluations, inconvertible currencies, discriminatory currency blocs, and a shortage of international reserves that transmitted depression across borders.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Jacques Rueff's contemporaneous critique (mid-1960s) argued the system survived only on accumulating American debt; Milton Friedman's floating-rate program documented that the parity structure lacked efficiency grounds; economic historians (Eichengreen, Bordo) date functional exhaustion to the mid-1960s on reserve-cover arithmetic; the Fund's own commissioned histories concede the adjustable-peg premise no longer matched capital flows by 1968. US Treasury attestations that the system remained sound were self-interested and stand against all of the above.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__overdetermined_composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Extractiveness ends at 0.74 because the transfer channel widened monotonically: dollar liabilities to official foreign holders rose from a small fraction of US gold cover in 1950 to several times the gold stock by 1971, so the gap between paper issued and metal pledged - the seigniorage margin - grew every year of the interval. Suppression (0.68) is authored as a raw structural property, unscaled by power or scope: it consists of legal bars on private gold holding for US persons, the Interest Equalization Tax and successive voluntary then mandatory capital-control programs, gold-pool market intervention, and diplomatic pressure on revaluation-resistant allies; it is structural throughout, not internalized. Theater ratio crosses 0.5 at interval end because late-regime maintenance turned performative: the gold pool's sales after 1965 defended a price everyone at the table knew was unsustainable, the March 1968 two-tier market openly split the official fiction from the market price, and the Smithsonian realignment was framed as temporary within weeks of signing. Accessibility collapse sits at 0.50: the alternatives (generalized floating, wider bands, crawling pegs, gold revaluation) were articulated and understood - Friedman from 1953, Rueff from the early 1960s - but politically foreclosed inside the managed consensus until the regime broke. Resistance at 0.62 reflects recurring speculative attacks (sterling 1967, gold 1968-69, the dollar 1971), the French critique, and academic dissent; coalition potential among the targets existed (the May 1971 joint float of surplus currencies was a partial realization) but collective action consistently arrived too late to matter. The three measurement series run on one shared time grid - every tracked metric is authored at all six examined points - and the trajectories are monotonic ratchets, not cycles: each leak widened the enforcement requirement, which widened the gap the next leak exploited. The rising base_extractiveness series is authored deliberately so the accumulation-abduction trigger can fire on this story. Fixing_cost is prohibitive because saving the anchor after 1968 required simultaneously contracting US fiscal policy, revaluing gold against allied resistance, and walling off a capital market that had already arbitraged around earlier controls; each component alone was unpayable for the only seat that could act, so the window was closed instead of the discipline restored.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat and the payer seats should compute different types from identical structural data. From the US Treasury/Fed position the anchor was a construction it owned: flexible enough to fund a war and a welfare expansion while formally pledged to gold, with a unilateral exit (the window) always in hand - coordination it administered. From the reserve-holder and deficit-country positions the same structure operated as compulsory holding of depreciating claims and deflationary adjustment imposed from outside. Wage earners experienced neither negotiation nor exit, only prices. The IMF seat adds an identity-lock dynamic: its mandate, staffing, and professional self-conception were constituted by the Articles it serviced, so advocating orderly exit was unthinkable from inside regardless of the ledger; if that institutional identity frame had broken earlier - staff reframing themselves as crisis managers rather than guardians of parities - the Fund's seat would compute with more mobility and the regime's terminal phase would likely have been shorter and less theatrical.
 *
 * DIRECTIONALITY LOGIC:
 *   The US Treasury/Fed seat is declared beneficiary and holds the agenda-setter role with arbitrage-grade exit, placing it nearest the beneficiary end of the directionality range; it is also the gain_flow seat, since seigniorage demonstrably accrued there and nowhere else. Foreign dollar reserve holders, deficit adjusters, wage earners, and price-capped gold producers are declared victims with constrained or trapped exit, placing them near the target end; the derivation amplifies their effective burden because their exits are locked or absent. Export-oriented surplus economies are the one seat where the automatic derivation would err: declared beneficiary, the chain would place them deep at the beneficiary end, but structurally they were compensated participants - export gains purchased with forced reserve accumulation, repeated revaluation pressure, and eventual devaluation losses on their dollar hoards - so an override moves the powerful atom to 0.38, near symmetric. Overrides are keyed by power atom, and the other candidate corrections collide with correctly-derived seats sharing their atoms (the IMF shares the institutional atom with the US seat; the floating-rate economists share the moderate atom with the deficit adjusters), so those seats ride the structural derivation or canonical fallback and the residual uncertainty is left visible rather than papered over. The kernel delta's phrase 'victim: monetary discipline' maps structurally onto the actor seats that bore the discipline's absence - savers, wage earners, and reserve holders - because abstract goods do not feed the directionality computation; only real actors collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - preventing a repeat of interwar monetary chaos - was substantially solved by the late 1950s: currencies were convertible on current account, trade multiplied, and no competitive devaluation cycle recurred. What the arrangement defended from roughly 1965 onward was no longer that problem but the US fiscal position the anchor's reserve role made financeable; the mandate had outlived its function while the structure persisted, which is the mandatrophy signature, and the theater-ratio crossing 0.5 marks the point where maintenance became proxy performance. The tangled_rope claim is what prevents mislabeling in both directions: reading the whole episode as pure coordination ignores the seigniorage channel and the adjustment asymmetry that made the regime extractive for a quarter century; reading it as pure extraction ignores the genuine reconstruction and trade coordination that made the extraction possible and widely tolerated. The five-questions mismatch (founding_problem_status dead against disappearance_verdict world_rearranges) flags the 1968-1971 phase specifically: a structure maintained theatrically after its function was achieved, dismantled only when the extraction seat's own costs finally exceeded the alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the overdetermined_composite_reading of the monetary_anchor_principle kernel; would the sibling readings (punctuated_swap_reading, triffin_inevitability_reading) instantiate structurally different constraints from the same kernel?',
    'Comparative classification of the sibling stories: if the punctuated reading computes a discrete enforcement-failure boundary at August 1971 while this reading computes continuous degradation through the 1960s, the kernel''s classification is reading-indexed and corpus analysis must compare readings rather than average them.',
    'Sibling readings would relocate the failure boundary (discrete 1971 event vs. continuous erosion), change which upstream streams are load-bearing, and shift per-seat types and epsilon trajectories; this file''s values hold only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Reading-indexicality of the transition classification within the monetary_anchor_principle kernel.').

omega_variable(
    causal_locus_disagreement,
    'Where do the readings locate the binding cause: a sufficient composite of four streams (this reading), a single reserve-currency dilemma (Triffin), or a discretionary regime swap (punctuated)?',
    'Counterfactual archival analysis: hold each stream fixed and vary the others; if removing any one stream restores sustainability, the composite-sufficiency claim fails and the single-mechanism reading gains ground.',
    'If Triffin alone suffices, the fiscal and Keynesian streams are accelerants rather than causes and epsilon is attributable to the dilemma''s arithmetic; if the swap was discretionary, epsilon concentrates at the 1971 decision point and the arrangement shades toward chosen rather than compelled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_locus_disagreement, conceptual, 'Location of the disagreement between sibling readings over the binding causal element.').

omega_variable(
    counterfactual_rescue_window,
    'Was a feasible policy mix available after 1968 (gold revaluation, capital controls, fiscal contraction) that would have preserved convertibility, making collapse contingent rather than overdetermined?',
    'Reconstruct the 1968-1971 policy menu against reserve-cover arithmetic and political feasibility constraints; test whether any combination closes the gold drain without unemployment costs exceeding the demonstrated tolerance of the Johnson and Nixon administrations.',
    'If a rescue existed, the transition embeds a choice component and the arrangement resembles a squandered transitional support; if none existed, structural overdetermination stands and epsilon reflects irreducible stream convergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_rescue_window, empirical, 'Whether the late-1960s rescue window was genuinely closed or closed only by political choice.').

omega_variable(
    rent_capture_distribution,
    'Did the anchor''s rents accrue predominantly to US fiscal capacity, or jointly to US seigniorage and surplus-economy export advantage via undervalued parities?',
    'Estimate real effective exchange-rate misalignment for Germany and Japan 1950-1971 and decompose seigniorage flows against export-subsidy transfers.',
    'Redistributes directionality between the agenda-setting and beneficiary seats; a large surplus-economy share would soften the extraction asymmetry and pull the computed type toward pure coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_capture_distribution, empirical, 'Distribution of the anchor''s rents across the capturing and compensated seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1950, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement_basis(mone_tr_t1950, observed).
narrative_ontology:measurement(mone_tr_t1958, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1958, 0.14).
narrative_ontology:measurement_basis(mone_tr_t1958, observed).
narrative_ontology:measurement(mone_tr_t1961, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1961, 0.2).
narrative_ontology:measurement_basis(mone_tr_t1961, observed).
narrative_ontology:measurement(mone_tr_t1965, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1965, 0.31).
narrative_ontology:measurement_basis(mone_tr_t1965, observed).
narrative_ontology:measurement(mone_tr_t1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1968, 0.46).
narrative_ontology:measurement_basis(mone_tr_t1968, observed).
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 1971, 0.52).
narrative_ontology:measurement_basis(mone_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t1950, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement_basis(mone_be_t1950, observed).
narrative_ontology:measurement(mone_be_t1958, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1958, 0.51).
narrative_ontology:measurement_basis(mone_be_t1958, observed).
narrative_ontology:measurement(mone_be_t1961, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1961, 0.57).
narrative_ontology:measurement_basis(mone_be_t1961, observed).
narrative_ontology:measurement(mone_be_t1965, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1965, 0.63).
narrative_ontology:measurement_basis(mone_be_t1965, observed).
narrative_ontology:measurement(mone_be_t1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1968, 0.69).
narrative_ontology:measurement_basis(mone_be_t1968, observed).
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 1971, 0.74).
narrative_ontology:measurement_basis(mone_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1950, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement_basis(mone_su_t1950, observed).
narrative_ontology:measurement(mone_su_t1958, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1958, 0.38).
narrative_ontology:measurement_basis(mone_su_t1958, observed).
narrative_ontology:measurement(mone_su_t1961, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1961, 0.47).
narrative_ontology:measurement_basis(mone_su_t1961, observed).
narrative_ontology:measurement(mone_su_t1965, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1965, 0.56).
narrative_ontology:measurement_basis(mone_su_t1965, observed).
narrative_ontology:measurement(mone_su_t1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1968, 0.64).
narrative_ontology:measurement_basis(mone_su_t1968, observed).
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 1971, 0.68).
narrative_ontology:measurement_basis(mone_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% Constraint family: monetary_anchor_principle decomposes into three readings with distinct epsilon and type profiles. This member (overdetermined composite) carries high epsilon and a tangled_rope claim because four causal streams converge and no single lever reverses the outcome. The punctuated_swap_reading locates the regime boundary in one 1971 decision and would carry a different enforcement-failure profile; the triffin_inevitability_reading reduces causation to reserve-currency arithmetic alone. Relational structure: the composite reading contains the Triffin stream as one component among four, exerting absorptive pressure on the single-mechanism sibling (influences), while remaining fully compatible with the discrete-swap sibling, since structural inevitability and discretionary timing answer different questions (coexists_with).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, powerful, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
