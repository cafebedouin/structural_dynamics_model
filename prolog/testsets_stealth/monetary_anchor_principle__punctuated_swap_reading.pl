% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Fiat-Dollar Anchor as Discrete Institutional Swap (Punctuated-Swap Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   This story instantiates the punctuated_swap_reading of the
 *   monetary_anchor_principle kernel: the end of gold convertibility was a
 *   discrete institutional choice taken on August 15, 1971, installing one
 *   regime in place of another by unilateral act. Per the kernel-reading
 *   rules, this file generates ONLY that reading as a clean,
 *   epsilon-invariant constraint: the standing arrangement under contest is
 *   the fiat-dollar anchor regime the swap installed, assessed by this
 *   reading's own lights, with epsilon authored for that arrangement and
 *   never for the gold-standard alternative the reading rejects. The other
 *   readings (overdetermined_composite_reading,
 *   triffin_inevitability_reading) are separate constraint files linked
 *   through network.affects_constraints; the contest between them is routed
 *   to omega variables, not folded into this classification. On the
 *   manifest's expected structural delta: the manifest hypothesized rope, but
 *   its own structural declarations (a named beneficiary in U.S. fiscal
 *   autonomy, a named victim in foreign dollar holders, moderate epsilon, and
 *   a regime that visibly requires active enforcement machinery) satisfy the
 *   canonical triple for a hybrid coordination/extraction structure, so the
 *   claimed_type is authored as tangled_rope. The claim and the metrics
 *   remain independently authored: the metrics describe the regime's actual
 *   operation as this reading sees it, and the engine computes per-seat types
 *   from the structural data without reference to the claim.
 *
 * KEY AGENTS:
 *   - - us_monetary_authorities: Agenda-setter and primary beneficiary (institutional/arbitrage) — issues the anchor asset, collects seigniorage, controls clearing access and swap lines
 *   - - foreign_reserve_holders: Primary target of the 1971-73 devaluation (organized/trapped) — bore the marked-down official dollar portfolios
 *   - - export_led_surplus_economies: Ongoing payer (organized/constrained) — accumulates dollars as growth strategy, bears valuation and sanctions risk
 *   - - emerging_market_dollar_debtors: Cyclical target (powerless/trapped) — absorbs the issuer's policy cycle through dollar-denominated debt
 *   - - multinational_trade_networks: Incidental beneficiary with minor cost exposure (powerful/mobile) — gains a common invoicing and settlement unit
 *   - - imf_membership: Formally consulted, effectively excluded (organized/constrained) — its par-value consultation rights were bypassed in 1971
 *   - - monetary_economists_bis: Analytical observer (analytical/analytical) — documents and evaluates the regime from outside the operating institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.64).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.68).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.64).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Fiat-Dollar Anchor as Discrete Institutional Swap (Punctuated-Swap Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "economic/political").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '9783a1c5-924e-4e01-b2e3-4967aac6e53e').
narrative_ontology:cs_kernel_codification('9783a1c5-924e-4e01-b2e3-4967aac6e53e', implicit).
narrative_ontology:cs_authority_grounding('9783a1c5-924e-4e01-b2e3-4967aac6e53e', practice).
narrative_ontology:cs_interpretation_layer_present('9783a1c5-924e-4e01-b2e3-4967aac6e53e').
narrative_ontology:cs_reading_relation('9783a1c5-924e-4e01-b2e3-4967aac6e53e', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('9783a1c5-924e-4e01-b2e3-4967aac6e53e', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('9783a1c5-924e-4e01-b2e3-4967aac6e53e', foundational, regime_transition_is_discretionary_act).
narrative_ontology:cs_axiom_status(regime_transition_is_discretionary_act, holdable).
narrative_ontology:cs_axiom_grounding('9783a1c5-924e-4e01-b2e3-4967aac6e53e', regime_transition_is_discretionary_act, empirically_contingent).
narrative_ontology:cs_axiom('9783a1c5-924e-4e01-b2e3-4967aac6e53e', secondary, anchor_arrangements_are_issuer_revisable).
narrative_ontology:cs_axiom_status(anchor_arrangements_are_issuer_revisable, holdable).
narrative_ontology:cs_axiom_grounding('9783a1c5-924e-4e01-b2e3-4967aac6e53e', anchor_arrangements_are_issuer_revisable, conventional).
narrative_ontology:cs_reference_frame('9783a1c5-924e-4e01-b2e3-4967aac6e53e', sovereign_discretionary_anchor).
narrative_ontology:cs_drift_state('9783a1c5-924e-4e01-b2e3-4967aac6e53e', contemporary_fragmentation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9783a1c5-924e-4e01-b2e3-4967aac6e53e', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, us_monetary_authorities).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, multinational_trade_networks).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_reserve_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, export_led_surplus_economies).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, emerging_market_dollar_debtors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, multinational_trade_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the anchor. The Treasury issues the debt the world stores as reserves; the Federal Reserve prices the currency, operates the swap lines and clearing access through which dollar liquidity reaches the rest of the world, and since 1971 has faced no external redemption obligation. The August 1971 suspension was decided over a secret weekend at Camp David and announced unilaterally, with allied governments informed hours before the public. The seat collects seigniorage, borrows in its own currency at global scale, and can grant or freeze access to dollar clearing. Its exit position is unique in the system: it issues the asset everyone else holds, so it carries no exchange risk on its own liabilities.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, us_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, us_monetary_authorities, beneficiary).

% Central banks and treasuries that accumulated dollar claims against the promise of conversion at thirty-five dollars per ounce. In August 1971 conversion was suspended without their consent; between 1971 and 1973 the dollar fell roughly a quarter to a third against gold and the major currencies, marking down their official portfolios. Selling out was not feasible then and is not feasible now: any large disposal depresses the value of the holdings that remain. They continue to hold the largest official dollar positions in the system and diversify only at the margin.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_reserve_holders, payer,
    organized, biographical, trapped, global).

% Run growth strategies built on selling into the U.S. market and managing exchange rates, which requires large-scale accumulation of dollar reserves. The accumulation is partly chosen — undervalued currencies powered their export booms — but the resulting portfolios are exposed to the issuer's inflation and sanctions decisions, and rapid unwinding would appreciate their currencies and stall the export engine. They absorb recurring valuation losses and hedge politically through reserve diversification talk more than practice.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, export_led_surplus_economies, payer,
    organized, biographical, constrained, global).

% Governments and firms that borrow in dollars because local-currency credit is unavailable or prohibitively expensive. When the issuing central bank tightens, their debts swell in local terms, capital withdraws, and they undergo contractions timed by a policy cycle they take no part in setting. There is no practical exit from dollar funding at their credit quality; their options are buffer accumulation, IMF programs, and requests for swap access they do not control.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, emerging_market_dollar_debtors, payer,
    powerless, biographical, trapped, global).

% Price and settle a large share of world trade in the anchor currency, gaining a common unit of account, deep hedging markets, and network familiarity that lowers transaction costs everywhere. They bear bid-offer spreads and episodic exchange-rate whiplash, but their mobility lets them re-invoice or hedge at will, and the shared currency spares them the cost of negotiating settlement bilaterally with every counterparty.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, multinational_trade_networks, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, multinational_trade_networks, payer).

% The treaty system the anchor grew out of gave members formal consultation rights over par values and scarce-currency procedures. The 1971 suspension was announced without invoking them, and the reformed par-value system those consultations were meant to produce was abandoned within eighteen months. Today the membership debates reserve-system reform in forums where voting weight tracks creditor status, leaving debtor majorities advisory rather than decisive on the arrangements their reserves sustain.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, imf_membership, excluded,
    organized, generational, constrained, global).

% Academic monetary economists, Bank for International Settlements researchers, and official-sector analysts who document reserve composition, invoicing shares, and swap-line usage, and who publish the historical and counterfactual analyses by which the 1971 decision and the regime it installed are evaluated from outside the operating institutions.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, monetary_economists_bis, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, us_monetary_authorities).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single global unit of account, settlement medium, and elastically supplied reserve asset, solving the collective-action problem in which no state wants to anchor to gold alone while all need deep, liquid reserves and a neutral invoicing currency for trade and debt.
% TRANSFER_FUNCTION: Moves seigniorage and below-market financing from foreign dollar holders and surplus exporters to the U.S. Treasury and Federal Reserve; moved the 1971-73 devaluation losses onto holders of the accumulated dollar overhang; and continuously transfers inflation, devaluation, and sanctions-freeze risk to reserve holders and dollar-debtors.
% ABSENT_VOICES: Foreign holders of the 1971 dollar overhang had no seat at the Camp David deliberations and learned of convertibility's end from television; the IMF consultation machinery their claims legally invoked was bypassed. Today, emerging-market borrowers exposed to the issuer's policy cycle and sanctioned states whose reserves can be frozen hold no vote in the deliberations that set their exposure — IMF and G20 voice is weighted toward creditors.
% DISAPPEARANCE_RATIONALE: Trade invoicing conventions, official reserve portfolios, dollar-denominated debt contracts, and the Treasury market's role as global collateral would all require simultaneous reconstruction. Any orderly replacement demands years of institution-building around a successor asset; disorderly exit would destroy holder wealth in the attempt. The world rearranges around whichever supplier emerges next, at enormous transition cost — which is precisely why the discrete-choice reading's reversibility premise is contested rather than exercised.
% FOUNDING_PROBLEM: In August 1971 the immediate problem was a run on U.S. gold: foreign-held dollar claims far exceeded the U.S. gold stock at thirty-five dollars per ounce, inflation was accelerating, and the Vietnam War and Great Society deficits could not be financed under convertibility discipline. The swap was built to suspend that constraint — announced as temporary, pending negotiation of a reformed par-value system.
% FOUNDING_PROBLEM_CORROBORATION: The historical record corroborates from outside the benefiting parties: Treasury archives and Volcker's contemporaneous option memoranda frame the measures as interim; the Smithsonian Agreement's collapse by March 1973 is documented in IMF histories; and the contemporaneous protests of European and Japanese central bankers, recorded in BIS files, attest the transfer they experienced. No party outside the U.S. authorities attests that the founding problem remains live, because its object — gold convertibility at thirty-five dollars — no longer exists to be defended.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.64 (end-state): the regime delivers genuine services (unit of account, elastic liquidity, deep collateral markets) while continuously transferring seigniorage, financing convenience, and inflation/devaluation risk to holders, on top of the discrete 1971-73 transfer. Suppression is 0.68: overt coercion returned to prominence once reserve freeze and clearing-access denial entered the toolkit, and large holders face genuine trap dynamics — selling depresses the value of remaining holdings. Theater is 0.36: the operational machinery is real, but a growing performative layer (communique diplomacy, strong-dollar rhetoric maintained alongside conspicuous privilege, summitry that produces announcements rather than alternatives) rides on top. Accessibility_collapse is 0.48 — alternatives exist (euro, gold, renminbi settlement, SDRs) but none fully substitutes, and understanding the trap does not open an exit. Resistance is 0.42 — de-dollarization initiatives, central-bank gold accumulation, and bilateral settlement schemes are real but have shifted reserve shares only at the margin. The measurement series run on ONE shared time grid (1971, 1973, 1980, 1985, 1999, 2008, 2015, 2025) with every tracked metric authored at every point, per the alignment rule. The suppression_requirement series is authored deliberately because this story traces enforcement-capacity change: heavy-handed controls at the swap (convertibility suspension, import surcharge, wage-price freeze), relaxation through the liberalization era to a 1999 trough, then reconstruction through swap lines, post-crisis dollar-clearing dominance, and sanctions infrastructure to a 2025 level matching the controls era — a U-shape, not monotonic drift. Extractiveness dips through the Great Moderation (credible low-inflation anchor, cheapest extraction phase) and rises again as privilege becomes conspicuous. Theater humps at the Smithsonian Agreement (billed as the greatest monetary agreement in history; collapsed in fourteen months) and the Plaza Accord (highly theatrical managed intervention). Claim and metrics are independent authored facts; the engine owns the arithmetic.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat, the arrangement is a service it operates and can revise at will — the issuer experiences no exchange risk on its own liabilities and reads the structure as provision. From the trapped historical-target seat (foreign_reserve_holders), the same structure registered as a discrete confiscation executed without consultation. From the ongoing-payer seats, it registers as a standing tax paid in valuation losses and policy-cycle exposure. Same-level lateral divergence is sharp: foreign_reserve_holders and export_led_surplus_economies both hold organized power, but the former is trapped (portfolio scale makes exit self-destructive) while the latter is merely constrained (accumulation is entangled with a growth strategy it could, at cost, unwind) — the difference is portfolio entanglement, not global standing. Emerging-market debtors experience the regime as weather rather than arrangement: they are exposed to decisions in which they hold no seat. The engine computes these divergences from power, exit, and directional data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   us_monetary_authorities sits at the beneficiary pole (d near 0): it declares the arrangements, collects the seigniorage, and holds arbitrage-grade exit because it issues the asset everyone else stores. foreign_reserve_holders sit at the target pole (d near 1): they bore the 1971-73 markdown and remain trapped by portfolio scale. export_led_surplus_economies sit high but not maximal: their dollar accumulation is partly self-chosen strategy, which damps effective targeting relative to a purely coerced holder. emerging_market_dollar_debtors sit near the target pole with no exit from dollar funding at their credit quality. multinational_trade_networks sit near the beneficiary pole with mobile exit — they gain the common unit and can hedge or re-invoice at will. imf_membership sits mid-range: partial voice, partial exposure. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and the one genuinely mixed seat (export_led_surplus_economies) is handled by the structural derivation rather than a blunt power-atom override.
 *
 * MANDATROPHY ANALYSIS:
 *   The genealogy is the interesting part. The swap was announced as a temporary suspension pending negotiation of a reformed par-value system; that founding problem (defend the dollar-gold parity, stop the gold drain, buy time for reform) is dead — the emergency passed within two years and the promised reform was negotiated and abandoned by March 1973. Yet the arrangement persists, having silently absorbed a replacement mandate (open-ended global liquidity provision under issuer discretion) without ever declaring the transition. The founding_problem_status x disappearance_verdict mismatch (dead x world_rearranges) will fire the capture/zombie flag, and the cross-check against the theater path matters: theater_ratio is 0.36 and the coordination function is real, so the structure reads as repurposed-and-functional rather than hollow-and-performed — a hybrid that persists by serving a new master, not a shell maintained by habit. The classification prevents two mislabels: calling the regime pure coordination ignores the expropriation the reading itself asserts and the standing privilege; calling it pure extraction ignores the genuine liquidity coordination nearly every participant still consumes. Mandatrophy is unresolved in the precise sense that the mandate changed without anyone resolving the change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_modal_status_of_1971_transition,
    'Was the 1971 transition a discrete discretionary institutional choice (this reading) or an overdetermined composite of structural pressures / a structurally inevitable Triffin outcome (the sibling readings)?',
    'Archival counterfactual analysis: whether a fiscally austere U.S. path could have maintained convertibility past August 1971, and whether the Camp David decision set contained live alternatives to suspension.',
    'If a sibling reading is correct, the transfer of 1971-73 shifts from choice-borne expropriation toward structural cost, this reading''s reversibility premise weakens, and classification migrates toward the siblings'' instantiations of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_modal_status_of_1971_transition, conceptual, 'Which reading of the monetary_anchor_principle kernel correctly characterizes the modality of the 1971 transition.').

omega_variable(
    regime_reversibility_lock_in,
    'Does this reading''s premise that the arrangement remains revisable in principle survive the network lock-in the fiat-dollar regime has since acquired?',
    'Longitudinal observation of de-dollarization episodes from the 2010s onward: whether any large holder exits dollar portfolios without self-harm, and at what portfolio share exit costs become prohibitive.',
    'If lock-in is decisive, the moderate-and-reversible epsilon premise fails and effective extraction rises for trapped seats; if revisability holds, the arrangement retains its choice-character and the agenda-setter seat bears continuing responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_reversibility_lock_in, empirical, 'Whether the swap-installed regime remains a revisable institutional choice or has become a locked equilibrium.').

omega_variable(
    expropriation_vs_escape_1971_73,
    'Did foreign holders of the dollar overhang lose more under the managed 1971-73 devaluations than they would have under continued convertibility or a disorderly collapse?',
    'Reconstruct reserve-composition counterfactuals from BIS and Treasury archives; compare realized 1971-73 valuation losses against modeled alternatives.',
    'Determines whether the swap''s transfer was expropriation (the victim declaration stands at full strength) or mutual escape from a worse equilibrium (victimhood attenuates and the coordination-side reading strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expropriation_vs_escape_1971_73, empirical, 'Magnitude and moral character of the 1971-73 transfer to foreign reserve holders.').

omega_variable(
    surplus_accumulation_consent_status,
    'Do export-led surplus economies hold dollar reserves as consenting strategy or under structural duress?',
    'Revealed-preference tests: whether these economies diversify reserves when exit costs fall, and how their accumulation responds to changes in U.S. policy credibility.',
    'If consent dominates, that seat''s effective targeting drops and the extraction asymmetry narrows toward the coordination side; if duress dominates, full-target treatment stands and the enforcement reading hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surplus_accumulation_consent_status, conceptual, 'Consent versus duress in the largest ongoing payer seat''s dollar accumulation.').

omega_variable(
    anchor_necessity_residual_extraction,
    'Does the world require some dominant reserve anchor as a network good, such that a residual extraction premium is irreducible coordination cost rather than removable rent?',
    'Comparative analysis of historical anchor systems (sterling era, gold-exchange era) to establish baseline privilege levels any anchor issuer collects.',
    'Sets the floor beneath which measured extraction reflects necessary coordination cost; above-floor excess is attributable to the incumbent''s discretionary position and supports the asymmetric-extraction reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(anchor_necessity_residual_extraction, conceptual, 'Irreducible coordination-cost floor versus discretionary rent in anchor issuance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 1971, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t1971, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1971, 0.2).
narrative_ontology:measurement(mone_tr_t1973, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1973, 0.24).
narrative_ontology:measurement(mone_tr_t1980, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(mone_tr_t1985, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1985, 0.34).
narrative_ontology:measurement(mone_tr_t1999, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 1999, 0.26).
narrative_ontology:measurement(mone_tr_t2008, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2008, 0.24).
narrative_ontology:measurement(mone_tr_t2015, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(mone_tr_t2025, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 2025, 0.36).

% Extraction over time
narrative_ontology:measurement(mone_be_t1971, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1971, 0.62).
narrative_ontology:measurement(mone_be_t1973, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1973, 0.66).
narrative_ontology:measurement(mone_be_t1980, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(mone_be_t1985, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(mone_be_t1999, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 1999, 0.5).
narrative_ontology:measurement(mone_be_t2008, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2008, 0.56).
narrative_ontology:measurement(mone_be_t2015, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(mone_be_t2025, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 2025, 0.64).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t1971, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1971, 0.72).
narrative_ontology:measurement(mone_su_t1973, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1973, 0.68).
narrative_ontology:measurement(mone_su_t1980, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(mone_su_t1985, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(mone_su_t1999, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 1999, 0.38).
narrative_ontology:measurement(mone_su_t2008, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2008, 0.52).
narrative_ontology:measurement(mone_su_t2015, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement(mone_su_t2025, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, petrodollar_settlement_arrangement).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the Nixon Shock / end of Bretton Woods' conflates three structurally distinct claims about the transition's modality, here split into three stories sharing the monetary_anchor_principle kernel prefix. The inevitability-flavored siblings (triffin_inevitability_reading, overdetermined_composite_reading) are upstream in argumentative order — their structural claims are routinely cited as evidence that the swap was forced — while this punctuated reading is downstream in evidentiary order: the archival record of deliberation and live alternatives constrains how much inevitability the siblings can claim. Each story carries its own epsilon, beneficiary/victim structure, and claimed type; all three link one another through network.affects_constraints. The petrodollar_settlement_arrangement edge records the downstream institutional coupling through which the swap-installed regime secured a recycling channel for surpluses after 1974.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
