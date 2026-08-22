% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__triffin_structural_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [RESOLVED - COLLAPSED 1971]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__triffin_structural_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__triffin_structural_reading
 *   human_readable: Dollar-Gold Convertibility (Triffin Structural Reading)
 *   domain: international_political_economy/monetary_history/international_law
 *
 * SUMMARY:
 *   Under the Bretton Woods Articles, the United States committed to convert
 *   foreign official dollar holdings into gold at 35 dollars per ounce, and
 *   every other member pegged its currency to the dollar. This story authors
 *   the TRIFFIN STRUCTURAL READING of that arrangement: the commitment was
 *   not a binding legal obligation to be honored (the strict reading) nor a
 *   conditional policy tool subordinate to domestic stability (the
 *   policy-flexible reading) but an inherently unsustainable design flaw,
 *   because supplying the world's reserve liquidity required expanding dollar
 *   liabilities against a fixed gold stock, which mechanically destroyed the
 *   confidence the commitment depended on. The colloquial label 'Bretton
 *   Woods convertibility' covers three structurally distinct claims with
 *   different epsilon values, different victim sets, and different
 *   classifications; per the epsilon-invariance principle this file authors
 *   only the third claim, and the siblings are separate stories linked
 *   through the network. The epsilon referent is the standing convertibility
 *   arrangement as this reading assesses it, not the reformed regime the
 *   reading endorsed. KEY AGENTS (by structural relationship):
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: Administrator and chief target (powerful/constrained) - set and defended the 35-dollar parity while absorbing gold losses and the subordination of domestic policy to external defense
 *   - creditor_nation_central_banks: Co-target (powerful/constrained) - accumulated dollar claims of shrinking cover as the price of defending their own parities
 *   - private_gold_hoarders: Arbitrage beneficiary (organized/arbitrage) - held the one-way option on official gold that the fixed price guaranteed
 *   - floating_regime_reform_coalition: Program beneficiary (organized/mobile) - converted each crisis into standing for the systemic-revision program and inherited the successor debate
 *   - non_g10_imf_membership: Excluded voice (powerless/trapped) - bore the system's consequences without a seat in its management
 *   - imf_research_department: Analytical observer (institutional/analytical) - measured the overhang and anticipated the confidence problem without holding a vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, 0.78).
domain_priors:suppression_score(dollar_gold_convertibility__triffin_structural_reading, 0.66).
domain_priors:theater_ratio(dollar_gold_convertibility__triffin_structural_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(dollar_gold_convertibility__triffin_structural_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__triffin_structural_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__triffin_structural_reading, "Dollar-Gold Convertibility (Triffin Structural Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__triffin_structural_reading, "international_political_economy/monetary_history/international_law").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__triffin_structural_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__triffin_structural_reading, 'e21c1c61-73ca-4206-8d23-65b6650e552b').
narrative_ontology:cs_kernel_codification('e21c1c61-73ca-4206-8d23-65b6650e552b', formalized).
narrative_ontology:cs_authority_grounding('e21c1c61-73ca-4206-8d23-65b6650e552b', lineage).
narrative_ontology:cs_interpretation_layer_present('e21c1c61-73ca-4206-8d23-65b6650e552b').
narrative_ontology:cs_reading_relation('e21c1c61-73ca-4206-8d23-65b6650e552b', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('e21c1c61-73ca-4206-8d23-65b6650e552b', dollar_gold_convertibility__policy_flexible_reading, influences).
narrative_ontology:cs_axiom('e21c1c61-73ca-4206-8d23-65b6650e552b', foundational, reserve_currency_design_inherently_unstable).
narrative_ontology:cs_axiom_status(reserve_currency_design_inherently_unstable, holdable).
narrative_ontology:cs_axiom_grounding('e21c1c61-73ca-4206-8d23-65b6650e552b', reserve_currency_design_inherently_unstable, empirically_contingent).
narrative_ontology:cs_axiom('e21c1c61-73ca-4206-8d23-65b6650e552b', foundational, systemic_revision_over_parity_defense).
narrative_ontology:cs_axiom_status(systemic_revision_over_parity_defense, holdable).
narrative_ontology:cs_axiom_grounding('e21c1c61-73ca-4206-8d23-65b6650e552b', systemic_revision_over_parity_defense, instrumental).
narrative_ontology:cs_reference_frame('e21c1c61-73ca-4206-8d23-65b6650e552b', collective_reserve_creation_order).
narrative_ontology:cs_drift_state('e21c1c61-73ca-4206-8d23-65b6650e552b', post_jamaica_accords_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e21c1c61-73ca-4206-8d23-65b6650e552b', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__triffin_structural_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, floating_regime_reform_coalition).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__triffin_structural_reading, private_gold_hoarders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and administered the 35-dollar gold parity and directed its defense: Treasury gold operations, Federal Reserve swap lines, the Interest Equalization Tax, voluntary credit restraint programs, and the U.S. share of the London Gold Pool. Administering the commitment consumed it: the gold cover ratio fell from roughly 60 percent of liquid dollar liabilities in the 1950s to about 22 percent by 1971. Each defense measure traded a domestic objective (full employment, Vietnam-era fiscal freedom, interest-rate autonomy) for external defense. Unilateral devaluation meant destroying the alliance monetary system the United States had built; continuing meant shipping gold to whoever asked. Exit looked like deflation at home or default abroad.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__triffin_structural_reading, us_monetary_authorities, payer).

% Accumulated dollar claims as the mechanical byproduct of defending their own parities: resisting appreciation meant buying dollars, and the purchases compounded into reserves far larger than U.S. gold cover could honor. Sterilizing the inflows cost domestic monetary control; converting en masse would destroy the system and their export markets overnight. Occasional defections bought relief at diplomatic price: France converted reserves into gold openly from 1965, Germany revalued and floated in 1969. Most held the claims and absorbed imported inflation from U.S. deficits instead.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, creditor_nation_central_banks, payer,
    powerful, biographical, constrained, global).

% Held a one-way option minted by the official price: buy gold at 35 dollars through official channels, hold it, and redeem into a private price that could only rise. Speculative waves repeatedly overwhelmed the London Gold Pool (1960, 1967-68), and after the March 1968 two-tier split the option persisted in institutional form: official holders could still convert at 35 while the free price climbed above 40. Every dollar of doubt about U.S. cover was a purchase signal.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, private_gold_hoarders, beneficiary,
    organized, immediate, arbitrage, global).

% The economists, officials, and Fund planners who diagnosed the design flaw publicly from 1960 onward and proposed substitutes: IMF reserve deposits, expanded SDR issuance, wider bands, managed floating. Each crisis converted their diagnosis into agenda standing; when the window closed in August 1971, their conceptual apparatus structured the successor debate, and the Fund gained the SDR issuance power they had designed. They collected legitimacy and institutional authority as the arrangement's dysfunction accumulated, without running it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, floating_regime_reform_coalition, beneficiary,
    organized, generational, mobile, global).

% The majority of Fund members had no seat in the Gold Pool, the G10, or the Working Party III conversations where the system was actually managed. They absorbed the consequences: commodity price swings from U.S. inflation, reserve scarcity managed on core-country terms, and conditionality written by creditors. They would have argued for quota-weighted reserve creation serving development needs; they were consulted after decisions, not before.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, non_g10_imf_membership, excluded,
    powerless, generational, trapped, global).

% Measured reserve adequacy ratios, published the dollar-overhang statistics, and staff papers anticipated the confidence problem years before the collapse. Its analyses circulated among the principals and informed the 1969 SDR amendment, but it held no vote on Gold Pool operations and no authority over U.S. defense policy.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__triffin_structural_reading, imf_research_department, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__triffin_structural_reading, private_gold_hoarders).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__triffin_structural_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplied the nominal anchor and the reserve liquidity that financed postwar reconstruction and the expansion of multilateral trade: fixed-but-adjustable parities ended the interwar cycle of competitive devaluation, and a convertible dollar gave every trading nation an internationally acceptable intermediary, solving the chronic reserve shortage collectively instead of bilaterally.
% TRANSFER_FUNCTION: Moved real resources and policy autonomy along three channels: gold moved from U.S. reserves to foreign official and private holders at the fixed 35-dollar price; inflationary finance capacity moved to the United States, whose deficits were financed by creditor acceptance of dollar claims; and the adjustment burden moved onto creditor nations, which absorbed dollar inflows and imported inflation to defend their own parities.
% ABSENT_VOICES: Non-G10 IMF membership, the majority of the world's countries, had no seat where the system was run; domestic publics in both blocs experienced the deflation-or-inflation tradeoff without representation in the central-bank diplomacy that made it. Both groups sat outside the executive committees of the Fund and the G10, engaged only after commitments were struck.
% DISAPPEARANCE_RATIONALE: When convertibility vanished in August 1971 the entire monetary architecture rearranged around the absence: exchange rates moved to generalized floating by 1973, gold repriced from 35 dollars toward 800 by 1980, the dollar remained the dominant reserve asset without any convertibility commitment, the SDR was created but stayed marginal, and a global inflationary decade followed. Every stakeholder's operating environment was rebuilt.
% FOUNDING_PROBLEM: Interwar monetary chaos: competitive devaluations, blocked currencies, chronic international reserve shortage, and beggar-thy-neighbor trade destruction. Bretton Woods built convertibility and stable parities to restore multilateral trade on predictable money.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is corroborated from outside the arrangement's beneficiary set: interwar monetary historiography documents the devaluation cycle and reserve starvation independently of any Bretton Woods constituency, and the problem's live status is attested by its recurrence in new form, the reserve-adequacy and global-safety-asset shortages documented after the Asian crisis and again after 2008 by researchers with no stake in the old regime. No party inside the successor floating regime needed the founding problem to be live for career or institutional reasons, which strengthens the corroboration.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__triffin_structural_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__triffin_structural_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__triffin_structural_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__triffin_structural_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__triffin_structural_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__triffin_structural_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__triffin_structural_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope on structural grounds independent of the metrics: the arrangement had a genuine coordination function (anchor plus reserve liquidity that rebuilt multilateral trade), and the same structure extracted from both principal parties, sustained by active enforcement (Gold Pool intervention, swap networks, capital controls, diplomatic pressure on converters). Extractiveness is high (0.78) because the reading assesses the arrangement as taxing both principals heavily: the United States paid in gold and policy autonomy, creditors paid in involuntary low-yield claims and imported inflation, and private actors skimmed the official price throughout. Suppression (0.66) is a raw structural property, unscaled by power or scope: it reflects the mature enforcement phase (controls, Gold Pool rationing, pressure on surplus countries), and the scalar sits near the 1963-1965 plateau of the suppression_requirement series rather than the terminal value, because the series traces an enforcement ratchet that crested and then collapsed - the only metric whose scalar deliberately does not match the endpoint, since the constraint died by abandonment rather than decay. Theater (0.52) reflects the endgame: after March 1968 the two-tier gold market maintained the official convertibility fiction while conceding its private-market death, and the final two years were largely declaratory defense. Accessibility collapse is low-moderate (0.38) because alternatives were visible and partially implemented (SDRs in 1969, wider bands, German floats) - understanding the flaw did not foreclose exits, it delayed them. Resistance is high (0.72): French conversion raids, German revaluations, sterling crises, and a sustained academic campaign all attacked the arrangement from inside its own membership. The measurement series run on one shared grid (eight points, three metrics, every metric authored at every point). The extractiveness trajectory is a crisis-ratchet rather than smooth drift: each speculative attack (1960, 1963-65, 1967-68, 1971) forced emergency defense, bought temporary calm, and left the baseline higher - the cycle's direction is monotonic even though its driver was episodic.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical facts. From the two payer seats the arrangement is a double bind: the United States experiences administering the commitment as consuming it, and creditors experience defending their parities as accumulating someone else's IOUs. From the reform coalition's seat the same structure is an evidence-generating machine - each crisis validated the diagnosis and expanded the program's audience. From the speculator's seat it is a priced option. The sharpest divergence is internal to a single agent: the U.S. seat holds both the agenda_setter role and the deepest payer position, which is precisely why the strict reading (honor the obligation) and this reading (the obligation is the disease) coexisted inside one treasury department for a decade. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. The two declared victim groups are the principals of the trilemma bind: us_monetary_authorities (constrained exit - deflation or default) and creditor_nation_central_banks (constrained exit - absorb or destroy the system), both mapping near the full-target end. The declared beneficiaries map near the subsidy end: private_gold_hoarders hold pure arbitrage positions (the strongest possible exit), and floating_regime_reform_coalition holds a mobile advocacy position collecting standing rather than rents. One override is declared: power_atom 'powerful' at d=0.78. The derivation would read the U.S. seat's agenda_setter role as beneficiary-leaning, because administrators typically sit on the collection side; in this reading that inference is exactly backwards - administering the flaw was the position of maximum exposure, and the creditor seats, though also powerful, are unambiguously targets. The override fixes both powerful seats at the reading's assessed target intensity; the atom contains exactly these two agents in this story, so the override is precise here even though the mechanism is coarse.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope prevents two symmetrical errors. Reading it as mountain would launder a human design into natural law: the Triffin arithmetic is compelling but the arrangement was enacted, amendable, and abandoned by decision - and the omega on inherent-versus-contingent unsustainability keeps the inevitability claim itself under test. Reading it as snare would erase the coordination content: the system genuinely financed reconstruction and trade expansion for fifteen years, and its victims were also its architects and managers, which no pure extraction story accommodates. Mandatrophy is resolved and dated: the credible-anchor mandate atrophied at the March 1968 two-tier split, after which the arrangement persisted in largely declaratory form for forty months - form outlived function, theater rose, and the end was administrative closure rather than negotiated retirement. The R5 interview records the founding problem as live rather than dead, so the mismatch consumer finds no zombie flag: the problem the arrangement was built for (stable international money under a liquidity-confident anchor) survives in transformed shape, while the arrangement itself is gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the convertibility kernel governs classification - and how would instantiating the strict or policy-flexible reading instead restructure this story''s victim and beneficiary sets?',
    'Comparative classification across the three sibling stories: locate where computed types converge and diverge, and trace each divergence to the structural element the readings disagree on (bindingness of the obligation, primacy of domestic stability, sustainability of the design).',
    'Under the strict reading, victimhood concentrates on U.S. policy autonomy and creditors become beneficiaries of externally imposed discipline; under the policy-flexible reading, the beneficiary seat moves to domestic constituencies of deficit countries. The both-principals-trapped structure is unique to this reading; if the corpus computes the siblings as materially different types, the kernel''s classification is reading-indexed rather than topic-level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of the dollar_gold_convertibility kernel; sibling readings instantiate different victim/beneficiary structures.').

omega_variable(
    inherent_vs_contingent_unsustainability,
    'Was convertibility''s collapse a structural necessity (the Triffin arithmetic of reserve growth against fixed gold stock) or contingent on identifiable policy choices (refusal to adjust, refusal to reprice gold, scale of U.S. deficits)?',
    'Counterfactual trajectory modeling: simulate gold-cover ratios under Rueff-style gold revaluation, under earlier and larger SDR substitution, and under disciplined U.S. adjustment paths, and compare against the observed cover-ratio collapse.',
    'If contingent, the arrangement is better read as a coordination mechanism that failed politically rather than a design fated to fail, and the mountain-like inevitability claim in this reading''s rhetoric loses force; if inherent, the structural reading is confirmed and the arrangement''s presentation of itself as a permanent order is exposed as the false-summit move this corpus watches for.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_contingent_unsustainability, empirical, 'Whether the unsustainability was built into the design or produced by choices made within it.').

omega_variable(
    net_us_position_ambiguity,
    'Was the United States a net victim of the arrangement (the Triffin double bind) or a net beneficiary (the Despres-Kindleberger-Salant counter-argument that the U.S. acted as a profitably intermediating world banker)?',
    'Balance-sheet reconstruction: aggregate U.S. gains (seigniorage on liquid liabilities, spread earnings on the maturity transformation, financing of external commitments with own IOUs) against losses (gold outflows, policy subordination, eventual devaluation trauma).',
    'A net-beneficiary finding moves the U.S. seat toward the beneficiary end of directionality and recasts the system as classic center-periphery extraction with a coerced periphery - a materially different classification than the symmetric double bind this reading authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_us_position_ambiguity, empirical, 'Whether the U.S. seat''s structural position was target or beneficiary is contested within the economics profession itself.').

omega_variable(
    sdr_revision_timing,
    'Could scaled SDR issuance under the 1969 First Amendment have restored sustainability in time, or was the reform instrument too small and too late by construction?',
    'Reserve-gap accounting: actual SDR allocations (roughly 9.5 billion dollars by 1972) set against the dollar overhang (above 40 billion) and the growth rate of world reserve demand.',
    'If timely large-scale revision was feasible, the constraint reads as a transitional support whose retirement was mismanaged rather than a flaw that had to run to collapse; if infeasible, collapse was the only exit and the structural reading''s necessity claim is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sdr_revision_timing, empirical, 'Whether the prescribed systemic revision was available in time to matter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__triffin_structural_reading, 1946, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triffin_conv_tr_t1946, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1946, 0.12).
narrative_ontology:measurement(triffin_conv_tr_t1950, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement(triffin_conv_tr_t1958, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(triffin_conv_tr_t1960, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1960, 0.26).
narrative_ontology:measurement(triffin_conv_tr_t1963, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1963, 0.33).
narrative_ontology:measurement(triffin_conv_tr_t1965, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(triffin_conv_tr_t1968, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1968, 0.48).
narrative_ontology:measurement(triffin_conv_tr_t1971, dollar_gold_convertibility__triffin_structural_reading, theater_ratio, 1971, 0.52).

% Extraction over time
narrative_ontology:measurement(triffin_conv_be_t1946, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1946, 0.3).
narrative_ontology:measurement(triffin_conv_be_t1950, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1950, 0.34).
narrative_ontology:measurement(triffin_conv_be_t1958, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1958, 0.42).
narrative_ontology:measurement(triffin_conv_be_t1960, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(triffin_conv_be_t1963, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1963, 0.58).
narrative_ontology:measurement(triffin_conv_be_t1965, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1965, 0.64).
narrative_ontology:measurement(triffin_conv_be_t1968, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1968, 0.71).
narrative_ontology:measurement(triffin_conv_be_t1971, dollar_gold_convertibility__triffin_structural_reading, base_extractiveness, 1971, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(triffin_conv_su_t1946, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1946, 0.25).
narrative_ontology:measurement(triffin_conv_su_t1950, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1950, 0.28).
narrative_ontology:measurement(triffin_conv_su_t1958, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1958, 0.38).
narrative_ontology:measurement(triffin_conv_su_t1960, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1960, 0.48).
narrative_ontology:measurement(triffin_conv_su_t1963, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1963, 0.6).
narrative_ontology:measurement(triffin_conv_su_t1965, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1965, 0.7).
narrative_ontology:measurement(triffin_conv_su_t1968, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1968, 0.55).
narrative_ontology:measurement(triffin_conv_su_t1971, dollar_gold_convertibility__triffin_structural_reading, suppression_requirement, 1971, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__triffin_structural_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, policy_flexible_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__triffin_structural_reading, impossible_trinity_structural_limit).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the 'Bretton Woods convertibility' label, per the epsilon-invariance principle. The colloquial concept covers three structurally distinct claims: (1) strict_convertibility_reading - a binding legal obligation constraining U.S. policy, with creditors as discipline beneficiaries; (2) policy_flexible_reading - a conditional commitment subordinate to domestic stability, with domestic constituencies as the protected seat; (3) this story - an inherently unsustainable design extracting from both principals until collapse, with the successor regime's coalition as residual beneficiary. Each carries its own epsilon, victims, and classification. The upstream claim (strict reading, highest textual confidence) influenced this one historically - the obligation's bindingness is what made the dilemma bite - and this reading in turn created the legitimacy conditions under which the flexible reading's suspension became thinkable in 1971. The impossible_trinity_structural_limit edge records the upstream structural dependency: the Triffin bind is the trinity instantiated for the reserve-currency supplier.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dollar_gold_convertibility__triffin_structural_reading, powerful, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
