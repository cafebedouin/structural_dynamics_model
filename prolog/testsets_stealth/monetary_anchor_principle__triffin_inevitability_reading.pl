% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma Reading of the Gold-Anchored Reserve Standard
 *   domain: economic/political/international finance
 *
 * SUMMARY:
 *   Under the Bretton Woods architecture, the United States pledged
 *   dollar-gold convertibility at $35 per ounce while the dollar served as
 *   the world's marginal reserve asset. Robert Triffin diagnosed in 1960 that
 *   these two commitments could not both survive growth: supplying world
 *   liquidity required persistent United States external deficits, and
 *   persistent deficits eroded the gold cover behind every outstanding dollar
 *   claim. This story instantiates ONE reading of the monetary-anchor kernel
 *   — the inevitability reading, on which the bind is a structural law of the
 *   arrangement and the 1971 abandonment was arithmetic executing itself. The
 *   epsilon referent is the standing gold-anchor arrangement as this reading
 *   assesses it: a genuine coordination order carrying a fatal design
 *   tension, not a designed extraction machine. The sibling readings
 *   (discrete-choice; multi-causal composite) are separate constraint files
 *   linked through the network block; their content is not folded into this
 *   classification. KEY AGENTS (by structural relationship): -
 *   us_gold_window_administrators: Agenda-setting seat
 *   (institutional/constrained) — administered the $35 parity and the gold
 *   window; held the suspension lever exercised in August 1971 -
 *   us_fiscal_spenders: Primary operational beneficiary
 *   (institutional/constrained) — deficits cleared as reserve accumulation
 *   abroad - european_creditor_central_banks: Primary target
 *   (organized/trapped) — held thinning convertible claims, unable to cash
 *   them without destroying the system - private_dollar_holders: Target with
 *   partial exit (powerful/mobile) — hedged into European currencies and
 *   Euromarkets - export_led_reserve_accumulators: Dual-positioned
 *   (organized/constrained) — collected liquidity, later absorbed devaluation
 *   - gold_producers: Peripheral beneficiary (organized/constrained) —
 *   exposed to official price policy - imf_parity_administration:
 *   Co-administrator (institutional/identity_locked) — mandate fused with the
 *   system it ran - monetary_economists_triffin_tradition: Analytical
 *   observer (moderate/analytical) — diagnosed the bind; reputation tracked
 *   its confirmation - us_wage_earners: Diffuse domestic target
 *   (organized/trapped) — bore the inflation side of the defense
 *
 * KEY AGENTS:
 *   - us_gold_window_administrators: agenda-setting seat (institutional/constrained) — administered parity and gold window; exercised the suspension lever in 1971
 *   - us_fiscal_spenders: primary operational beneficiary (institutional/constrained) — deficit financing accommodated by reserve accumulation abroad
 *   - european_creditor_central_banks: primary target (organized/trapped) — held thinning convertible claims; conversion threats were the only lever
 *   - private_dollar_holders: target with partial exit (powerful/mobile) — flight into DM, francs, and Eurodollars accelerated the drain
 *   - export_led_reserve_accumulators: dual-positioned (organized/constrained) — Germany and Japan collected liquidity and later absorbed devaluation
 *   - gold_producers: peripheral beneficiary (organized/constrained) — lobbied for revaluation; gained massively post-float
 *   - imf_parity_administration: co-administrator (institutional/identity_locked) — administered adjustment rules; mandate fused with the system
 *   - monetary_economists_triffin_tradition: analytical observer (moderate/analytical) — diagnosed the bind in 1960; vindicated by events
 *   - us_wage_earners: diffuse domestic target (organized/trapped) — bore accelerating inflation from 1965 onward
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.24).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.58).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.24).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.56).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma Reading of the Gold-Anchored Reserve Standard").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "economic/political/international finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__triffin_inevitability_reading).
domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, 'b3e74a0b-0d89-4fad-a18e-270508c0f1c9').
narrative_ontology:cs_kernel_codification('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', formalized).
narrative_ontology:cs_authority_grounding('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', expertise).
narrative_ontology:cs_interpretation_layer_present('b3e74a0b-0d89-4fad-a18e-270508c0f1c9').
narrative_ontology:cs_reading_relation('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', foundational, gold_exchange_standard_arithmetically_self_limiting).
narrative_ontology:cs_axiom_status(gold_exchange_standard_arithmetically_self_limiting, holdable).
narrative_ontology:cs_axiom_grounding('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', gold_exchange_standard_arithmetically_self_limiting, empirically_contingent).
narrative_ontology:cs_axiom('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', secondary, global_liquidity_supply_outweighs_anchor_defense).
narrative_ontology:cs_axiom_status(global_liquidity_supply_outweighs_anchor_defense, holdable).
narrative_ontology:cs_axiom_grounding('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', global_liquidity_supply_outweighs_anchor_defense, instrumental).
narrative_ontology:cs_reference_frame('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', anchor_as_structural_law_of_monetary_orders).
narrative_ontology:cs_drift_state('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', post_1971_historiographic_revision, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b3e74a0b-0d89-4fad-a18e-270508c0f1c9', '2026-06-12T09:14:32Z').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, us_fiscal_spenders).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, export_led_reserve_accumulators).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__triffin_inevitability_reading, gold_producers).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, european_creditor_central_banks).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, private_dollar_holders).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, us_wage_earners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, export_led_reserve_accumulators).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_thesis).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, fixed_parity_liquidity_exhaustion_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Treasury and Federal Reserve officials who maintained the $35 parity, managed the gold stock against foreign conversion demands, and operated the defense instruments of the 1960s. Each year they chose between tightening the defense and accepting further erosion of cover. They held the legal power to suspend convertibility throughout, and exercised it on August 15, 1971 after concluding that continuation was untenable.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_gold_window_administrators, agenda_setter,
    institutional, generational, constrained, global).

% Congress and the executive financed Vietnam War expenditure and Great Society programs through deficits that foreign monetary authorities were compelled to accumulate as reserve claims. The arrangement cleared this borrowing without visible penalty for over a decade; the bill arrived afterward as the devaluation and inflation that followed suspension. Fiscal retrenchment was politically unavailable at every decision point.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_fiscal_spenders, beneficiary,
    institutional, biographical, constrained, national).

% The Bundesbank, Banque de France, and peer institutions accumulated dollar claims as the byproduct of defending their own parities. Each held a convertible promise whose gold backing thinned yearly. Demanding full conversion would have exhausted United States gold within months and destroyed the export markets their economies depended on; threatening conversion piecemeal, as France did, was the only usable lever.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, european_creditor_central_banks, payer,
    organized, generational, trapped, continental).

% Multinational corporations and international banks holding working balances in dollars. They watched cover ratios deteriorate and shifted holdings into Deutschemarks, Swiss francs, and the fast-growing Eurodollar market. This flight was rational self-protection, and it simultaneously accelerated the drain on United States gold.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, private_dollar_holders, payer,
    powerful, biographical, mobile, global).

% Germany and Japan ran managed-undervaluation growth strategies that required absorbing dollar inflows as reserves. The inflows financed their export expansion; the same claims lost real value when the parity system dissolved. Revaluing early would have slowed their export engines; waiting compounded their exposure to the eventual settlement.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, export_led_reserve_accumulators, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__triffin_inevitability_reading, export_led_reserve_accumulators, payer).

% South African, Soviet, and Canadian miners sold into a market whose official price was pinned at $35 per ounce. They lobbied for an official price increase throughout the 1960s, watched the free-market price detach upward after 1968, and gained enormously when the price floated freely after 1971.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, gold_producers, beneficiary,
    organized, generational, constrained, global).

% The Fund administered the parity-change and adjustment rules the architecture ran on, brokered the Special Drawing Right creation of 1969 as a substitute reserve asset, and operated throughout subordinate to the United States' position as issuer. Its mandate and institutional relevance were fused with the system it administered; its staff could not conceive of their function apart from it.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, imf_parity_administration, agenda_setter,
    institutional, generational, identity_locked, global).

% The profession around Triffin's 1960 diagnosis mapped the arithmetic of the bind, proposed collective-reserve substitutes that became the SDR, and saw its predictive standing rise sharply when the predicted collapse arrived on schedule. Their analytical position sits outside the arrangement's operation; their professional reputation tracked its fate.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, monetary_economists_triffin_tradition, observer,
    moderate, generational, analytical, global).

% American households paid the domestic price of the liquidity-supply deficits: inflation accelerated from 1965 onward, eroding real wages while the external commitments were defended. They had no exit from the domestic price level and no seat in the councils deciding the tradeoff between external defense and internal purchasing power.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_wage_earners, payer,
    organized, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__triffin_inevitability_reading, us_fiscal_spenders).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__triffin_inevitability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the interwar coordination failure: a single settlement asset and a fixed parity grid gave trading nations predictable exchange rates, elastic world liquidity anchored to a scarce asset, and a disciplinary commitment device against competitive devaluation.
% TRANSFER_FUNCTION: Moved real resources from the rest of the world to United States spenders in exchange for dollar claims (settlement deficits), and moved reserve liquidity from the United States to reserve-short economies; in the terminal phase it moved purchasing power from every holder of dollar claims through devaluation and inflation.
% ABSENT_VOICES: Foreign creditor central banks and private dollar holders had no seat when suspension was decided: the Camp David weekend of August 13-15, 1971 gathered fifteen American advisers and no foreign representative, and creditor governments learned of the default on television. Economies pegging to the dollar in Latin America and elsewhere were absent entirely. French advocacy for gold revaluation was heard inside the councils but overruled without a negotiated settlement.
% DISAPPEARANCE_RATIONALE: Every named seat's position was constituted by the anchor: creditor banks' reserve portfolios, exporters' exchange-rate strategies, the Fund's adjustment machinery, gold producers' price expectations, and the profession's diagnostic authority all reorganized after August 1971. Floating rates, the Euromarket explosion, and the inflation decade are the rearrangement.
% FOUNDING_PROBLEM: The interwar disorder: competitive devaluations, beggar-thy-neighbor trade policy, a world gold shortage strangling recovery, and exchange-rate chaos that suppressed trade. The 1944 design promised liquidity with discipline.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: interwar testimony assembled at the 1922 Genoa and 1933 London conferences, Keynes's and White's 1942-44 planning memoranda, and post-hoc monetary history (Eichengreen's interwar syntheses, Bordo's retrospective assessments) all attest that the original problem was real and is now solved by other means. No living party claims the gold-scarcity liquidity problem still binds.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.24, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.24 at interval end) because nothing in the arrangement was designed to take from anyone: the seigniorage inherent to reserve provision sits at or below the coordination floor for planetary monetary infrastructure for most of the interval, and the metric rises only as terminal devaluation and inflation crystallize losses onto dollar holders. Suppression (0.58) is raw and unscaled: the record is an enforcement ratchet — the Interest Equalization Tax (1963), voluntary then mandatory capital controls (1965-68), the Gold Pool's market interventions (1961-68) — each layer buying time and hardening the defense. Theater (0.41) rises late: the two-tier gold market after March 1968 was an official fiction, and the Smithsonian realignment of December 1971 — announced as the greatest monetary agreement in history — unraveled in fourteen months. Accessibility collapse (0.74) is high but short of natural-law completeness: inside the architecture the arithmetic closes every door once understood, yet cross-architecture exits (floating, revaluation) remained open and were ultimately taken. Resistance (0.56) is far above natural-law levels: a decade of institutional energy went into fighting the bind. Claim and metrics are authored independently: the reading claims structural necessity (mountain, emergent), and the metrics report an enforcement-dependent, resistance-meeting, terminally theatrical operation — that divergence is the datum this corpus exists to take. All three series share one six-point grid (1958, 1961, 1964, 1967, 1969, 1971), each metric authored at every point; the rising base_extractiveness trajectory on a mountain claim is expected to trip the T17 accumulation hypothesis, which is the correct diagnostic here.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same arithmetic differently. The creditor central banks (trapped) experience the bind as confiscatory drift — reserves they must hold lose cover yearly, and demanding conversion would detonate the system they depend on. Private dollar holders (mobile) experience it as a hedging problem — they exit into Deutschmarks and Euromarkets, and their exit accelerates the drain. The fiscal spenders experience the operation as pure subsidy — deficits clear without penalty until the day they do not. The window administrators experience it as an impossible defense — every instrument (pool, controls, swaps) buys time while worsening the terminal reckoning. The engine computes these divergences from power, exit, and declaration data; the authored mountain claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map cleanly onto the derivation chain, so no overrides are authored. European creditor central banks and wage earners sit nearest the full-target end: declared victims with trapped exit. Private dollar holders are victims with mobile exit — high but discounted, since arbitrage-grade flight dampens effective extraction. Fiscal spenders sit nearest the beneficiary end: declared beneficiaries whose deficits the arrangement accommodated. Export-led accumulators derive low-to-mid directionality from dual declaration (beneficiary primary, payer secondary) — they collected liquidity and later ate devaluation. Window administrators derive mid-range directionality from the fallback (no declaration): they administered the bind and absorbed its terminal political cost. Scope amplification applies modestly: the arrangement is global, so verification of cover ratios was public while enforcement across jurisdictions was not.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (gold-scarcity-constrained liquidity) is dead and the world rearranged — the mismatch consumer will flag dead-problem-plus-rearrangement, but the arrangement did not persist past its function; it terminated. Theater rose only in the terminal episodes (two-tier gold market, Smithsonian announcement), reaching 0.41 at the end — a death performance, not inertial maintenance, and below the threshold at which theatrical maintenance sustains an otherwise functionless structure. The classification prevents two errors: reading the 1958-71 defense apparatus as pure extraction (its costs were systemic fallout of an arithmetic bind, not designed rent collection — extractiveness stays low and near the global-infrastructure coordination floor for most of the interval) and reading the collapse as costless nature (three named victim seats bore crystallized losses, and the receipt surface names where the offsetting gains landed).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    architecture_contingency_vs_natural_law,
    'Is the Triffin bind a natural law of any reserve-currency arrangement, or a consequence of contingent 1944 design choices — gold convertibility at fixed parity plus a single national currency as world reserve?',
    'Comparative institutional analysis: specify a bancor-style symmetric reserve or commodity-basket anchor and test whether the same exhaustion arithmetic arises; survey interwar and post-1971 designs for structurally analogous binds.',
    'If the bind follows only from the specific 1944 parameters, the arrangement is a constructed order mislabeled as natural law — the mountain claim loses its naturality warrant and the false-summit signature becomes the live classification path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architecture_contingency_vs_natural_law, conceptual, 'Whether the dilemma is architecture-independent law or a contingent-design artifact.').

omega_variable(
    counterfactual_gold_devaluation_viability,
    'Could a timely official gold-price increase — the standing French proposal of 1965-68 — have restored reserve cover and preserved the anchor, making abandonment a choice rather than a necessity?',
    'Archival reconstruction of the 1965-68 option papers (Treasury, Federal Reserve, Executive Office) plus counterfactual arithmetic: required parity change versus gold-stock growth and speculative-flow estimates.',
    'A viable devaluation path converts the transition from structural necessity to foregone choice, collapsing this reading''s mountain claim and shifting weight to the punctuated-swap sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_gold_devaluation_viability, empirical, 'Whether a feasible parity adjustment existed inside the architecture.').

omega_variable(
    inevitability_narrative_exculpation_function,
    'Does the inevitability framing perform an exculpatory function for the decision-makers who suspended convertibility, insulating them from accountability for the timing and manner of the default?',
    'Rhetorical and memoir analysis of 1971-73 justification discourse (Connally-era statements, Camp David participants'' accounts) set against contemporaneous internal acknowledgments of open alternatives.',
    'If exculpation is load-bearing, identifiable agents collect reputational rents from the natural-law framing even though the underlying arithmetic extracted little — supporting reclassification toward a hybrid coordination/extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inevitability_narrative_exculpation_function, empirical, 'Whether the natural-law framing shelters decision-makers from blame.').

omega_variable(
    new_triffin_generalization_scope,
    'Does the post-1971 recurrence of Triffin-type binds under pure fiat (safe-asset shortages, post-2008 reserve asymmetries) confirm the dilemma as a timeless structural law, or reveal a recurring design pattern re-chosen in each era?',
    'Cross-era comparison of reserve-supply mechanisms against confidence constraints; test whether fiat-era dynamics satisfy the same exhaustion inequality the gold-anchor version did.',
    'Confirmation strengthens emerges_naturally and the mountain claim; disconfirmation recasts the dilemma as one configuration of a choosable family, weakening this reading''s necessity premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(new_triffin_generalization_scope, empirical, 'Whether the dilemma generalizes beyond the gold-anchor architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(mone_tr_t0, observed).
narrative_ontology:measurement(mone_tr_t3, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement_basis(mone_tr_t3, observed).
narrative_ontology:measurement(mone_tr_t6, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 6, 0.14).
narrative_ontology:measurement_basis(mone_tr_t6, observed).
narrative_ontology:measurement(mone_tr_t9, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 9, 0.22).
narrative_ontology:measurement_basis(mone_tr_t9, observed).
narrative_ontology:measurement(mone_tr_t11, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 11, 0.3).
narrative_ontology:measurement_basis(mone_tr_t11, observed).
narrative_ontology:measurement(mone_tr_t13, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 13, 0.41).
narrative_ontology:measurement_basis(mone_tr_t13, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement_basis(mone_be_t0, observed).
narrative_ontology:measurement(mone_be_t3, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 3, 0.08).
narrative_ontology:measurement_basis(mone_be_t3, observed).
narrative_ontology:measurement(mone_be_t6, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement_basis(mone_be_t6, observed).
narrative_ontology:measurement(mone_be_t9, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 9, 0.15).
narrative_ontology:measurement_basis(mone_be_t9, observed).
narrative_ontology:measurement(mone_be_t11, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 11, 0.19).
narrative_ontology:measurement_basis(mone_be_t11, observed).
narrative_ontology:measurement(mone_be_t13, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 13, 0.24).
narrative_ontology:measurement_basis(mone_be_t13, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(mone_su_t0, observed).
narrative_ontology:measurement(mone_su_t3, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 3, 0.15).
narrative_ontology:measurement_basis(mone_su_t3, observed).
narrative_ontology:measurement(mone_su_t6, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 6, 0.24).
narrative_ontology:measurement_basis(mone_su_t6, observed).
narrative_ontology:measurement(mone_su_t9, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 9, 0.36).
narrative_ontology:measurement_basis(mone_su_t9, observed).
narrative_ontology:measurement(mone_su_t11, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 11, 0.48).
narrative_ontology:measurement_basis(mone_su_t11, observed).
narrative_ontology:measurement(mone_su_t13, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 13, 0.58).
narrative_ontology:measurement_basis(mone_su_t13, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'why Bretton Woods ended' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file authors the inevitability reading (epsilon very low: structural necessity, no policy choice available, no designed extractor). The punctuated_swap_reading authors the discrete-choice claim (epsilon materially higher: a decision imposes its costs on those never consulted). The overdetermined_composite_reading authors the multi-causal claim (intermediate epsilon: plural pressures, each partially avoidable alone). Upstream/downstream structure: the composite reading cites this reading's arithmetic as one causal strand, so this file influences it; the swap reading uses inevitability-talk as its foil, and the two readings' core premises are contradictories over the same event's modality. Downstream fiat-regime constraint stories (seigniorage arrangements, petrodollar recycling) would attach here as dependents once authored.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
