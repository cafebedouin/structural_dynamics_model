% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_sovereignty_defense, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Monetary Sovereignty Defense Constraint
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   Bretton Woods, established at the 1944 conference, created a monetary
 *   system where national currencies were pegged to the U.S. dollar at fixed
 *   rates, and the dollar was pegged to gold at $35/ounce. This reading — the
 *   'sovereignty defense' reading — frames the constraint as operating to
 *   defend national monetary sovereignty by providing a stable anchor that
 *   permits countries to maintain capital controls and pursue domestic policy
 *   objectives without constant renegotiation of exchange rates. However, the
 *   structural data reveals a different pattern: the constraint operates
 *   asymmetrically. The U.S., as the reserve-currency issuer and gold-peg
 *   anchor, retains substantial monetary autonomy (it can inflate the dollar
 *   without being forced to surrender gold, while others must surrender gold
 *   if reserves drain). Non-reserve-currency states experience the constraint
 *   as external discipline: they must maintain fixed pegs, hold dollar
 *   reserves, and surrender independent monetary policy if it diverges from
 *   U.S. conditions. The sovereignty being defended is primarily that of the
 *   reserve-currency center; peripheral economies experience it as
 *   sovereignty forfeited. This reading interprets the constraint as
 *   deliberately structured to preserve U.S. monetary dominance while
 *   legitimating it under the language of 'international stability' and
 *   'national autonomy.' The constraint is authored as tangled_rope because
 *   it genuinely solves a coordination problem (preventing competitive
 *   devaluation, establishing a common unit of account) AND asymmetrically
 *   extracts from non-reserve states to the benefit of the reserve-currency
 *   issuer.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.68).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.71).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.68).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Monetary Sovereignty Defense Constraint").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '7d6c00a9-45ea-48dd-8da5-dc9010a72a5c').
narrative_ontology:cs_kernel_codification('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', formalized).
narrative_ontology:cs_authority_grounding('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', extraction).
narrative_ontology:cs_interpretation_layer_present('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c').
narrative_ontology:cs_reading_relation('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', foundational, fixed_peg_enables_national_monetary_sovereignty).
narrative_ontology:cs_axiom_status(fixed_peg_enables_national_monetary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', fixed_peg_enables_national_monetary_sovereignty, instrumental).
narrative_ontology:cs_axiom('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', secondary, gold_anchor_requires_discipline_asymmetry).
narrative_ontology:cs_axiom_status(gold_anchor_requires_discipline_asymmetry, overridden).
narrative_ontology:cs_axiom_grounding('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', gold_anchor_requires_discipline_asymmetry, empirically_contingent).
narrative_ontology:cs_reference_frame('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', stable_multilateral_monetary_order_with_capital_controls).
narrative_ontology:cs_drift_state('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', dollar_gold_convertibility_breakdown_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('7d6c00a9-45ea-48dd-8da5-dc9010a72a5c', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authority).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuers).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, gold_standard_adherent_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, gold_standard_adherent_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes reserve currency status and enforces gold peg at $35/ounce. Sets the framework governing capital flows and sets monetary policy without external constraint. Collects seigniorage on reserve balances held globally and benefits from ability to finance deficits in own currency. Enforces the treaty rules that bind others while maintaining exemption from external discipline.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Must maintain fixed exchange rates to the dollar and hold gold/dollar reserves to back their currencies. Cannot pursue independent monetary policy divergent from dollar conditions without depleting reserves or triggering capital flight. Face external discipline whenever internal policy objectives conflict with maintaining the peg. Exit requires either defaulting or breaking the treaty, both carrying severe international consequences.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, generational, constrained, global).

% Locked into the Bretton Woods system with minimal discretion. Must keep dollar reserves even when domestic inflation demands credit expansion. Transmission of U.S. monetary shocks arrives as imported inflation or forced reserve depletion. Have no seat at treaty administration and cannot modify the terms; exit is economically impossible as it means exclusion from international capital markets.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies, payer,
    powerless, biographical, trapped, global).

% Holds anchor position in the monetary hierarchy. Benefits from seigniorage and from ability to set rates for the entire system. Credibility anchored by gold standard commitment, but gold commitment is asymmetric: the reserve issuer can always demand gold at peg price (deflationary option), while others cannot force conversion if reserves are depleted.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuers, beneficiary,
    institutional, generational, mobile, global).

% Publicly committed to gold standard to signal monetary discipline and attract capital inflows. Must actually surrender gold if reserves drain, which forces deflationary policy. The gold anchor is the source of their credibility (they cannot inflate) but also their constraint; reserve depletion triggers hard adjustment while the U.S. (as gold standard issuer, not holder) can inflate and let the gold constraint break asymmetrically.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gold_standard_adherent_states, payer,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, gold_standard_adherent_states, beneficiary).

% Administers Bretton Woods rules, adjudicates disputes over exchange rate adjustments, provides temporary bridge financing. Acts as the enforcement arm of the treaty, but the U.S. holds veto power over major decisions; IMF authority is real but structurally subordinate to the reserve currency issuer's interests.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund, agenda_setter,
    institutional, generational, analytical, global).

% Would be excluded from participation in treaty enforcement, but are the mechanism that enforces it: expectations of exchange rate stability or collapse drive capital flows and discipline member states' policy choices. Their exclusion from the rule-setting framework means treaty architects cannot bind speculative behavior; speculators effectively veto monetary independence by threatening capital flight.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, capital_account_speculators, excluded,
    powerful, immediate, mobile, global).

% Analyzes whether the constraint protects national sovereignty (sovereignty defense reading: pegged rates prevent competitive devaluation and give discipline-seekers credible commitment) or undermines it (embedded liberalism reading: capital flows restrict policy space; neoliberal reading: intervention is restricted, freeing markets). The reading disputed determines which observation is true.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, political_economists_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authority).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods solves a coordination problem among trading nations: prevent competitive devaluation spirals from the interwar period, provide a common unit of account for international commerce, create a stable framework for capital movements that permits trade finance without constant renegotiation of exchange rates.
% TRANSFER_FUNCTION: Transfers policy autonomy from peripheral economies to the center (U.S./reserve-currency issuers). Non-reserve states must maintain pegged rates and hold dollar reserves, which constrains their independent monetary policy. The center (U.S.) obtains the benefit of being able to set monetary policy for the whole system and collect seigniorage; peripheral states pay by surrendering policy discretion and absorbing transmitted shocks.
% ABSENT_VOICES: Domestic constituencies in non-reserve states that would want monetary independence, full-employment policymakers in peripheral economies, capital speculators (excluded from rule-making but de facto enforcement agents). These voices are locked out: they cannot modify the treaty framework, cannot represent alternative monetary visions (e.g. full employment over price stability), and in the case of speculators, are blamed for crisis transmission rather than consulted about the system's design.
% DISAPPEARANCE_RATIONALE: If Bretton Woods vanished, exchange rates would float, monetary policies would diverge, capital would reallocate to reflect risk differentials rather than fixed-parity expectations. Peripheral economies would regain discretion to pursue full employment or developmentalist policies without external discipline; the U.S. would lose seigniorage and the ability to finance deficits in its own currency without inflation transmission. The entire architecture of post-war economic governance reorganizes.
% FOUNDING_PROBLEM: Prevent return to 1930s competitive devaluation and competitive blocs; create stable exchange rates and common pricing for international trade; provide a framework where countries can maintain capital controls to pursue domestic policy objectives without destabilizing the international system.
% FOUNDING_PROBLEM_CORROBORATION: Keynes and White, the founders, emphasized the commitment to national policy autonomy within a stable external framework. However, post-war peripheral economists and 1960s-1970s development theorists attested that the actual constraint operated to foreclose autonomy. IMF structural adjustment conditionality (post-Bretton Woods but continuous with its logic) and the Triffin dilemma evidence both the founding intention (stability + autonomy) and its failure (stability required subordination to the center).
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness runs at 0.68 at interval end (1971), having risen from 0.42 at treaty inception (1944). This rising trajectory reflects accumulating extraction as the system matured: in 1944, it was genuinely a negotiated coordination framework with mutual commitments. By 1971, the U.S. had grown into exorbitant privilege (ability to finance deficits in its own currency, collect seigniorage, maintain inflation while forcing deflationary adjustment on others), while peripheral economies faced increasingly binding external discipline as dollar reserves proved inadequate during the Korean War, Suez crisis, and Vietnam War spending. Suppression runs parallel to extractiveness (0.48→0.71) because the constraint's persistence depends on enforcing the fixed-peg commitment: IMF conditionality, capital controls to prevent runs, and ultimately the credible threat of expulsion from the dollar zone. Theater rises more gradually (0.18→0.42) because the coordination narrative remained partially real (trade did stabilize, competitive devaluation did not return) even as the extraction function grew. The constraint never becomes pure snare because the coordination gains are genuine; but as extractiveness rises and suppression hardens, the coordination function increasingly serves as cover for the extraction. Accessibility collapse is moderate (0.58) because alternatives existed theoretically (floating rates, regional blocs, gold standard revival) but were actively suppressed and politically expensive; resistance is high (0.64) because peripheral economists, full-employment advocates, and French policymakers actively questioned the regime, though they lacked the power to modify it.
 *
 * PERSPECTIVAL GAP:
 *   The reserve-currency-issuer seat and the peripheral-economy seat should compute radically different types from the same structural data. From the U.S. seat, looking at 0.68 extractiveness, this appears to be the price of coordinating a stable global monetary system in which the U.S. can project power and finance its deficits — a snare-to-rope computation on the beneficiary end (low d means high/negative χ). From the peripheral seat, the same 0.68 extractiveness is the price of subordination to an external master — a pure snare or tangled-rope computation on the payer end (high d means high χ). The engine's per-seat computation should capture this divergence: one seat (U.S.) sees 'we're maintaining order,' another seat (peripheral) sees 'we're being disciplined.' The engine computes this from the stakeholder positions and directionality; I author the structural data that makes this divergence predictable.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S./reserve-currency-issuer seat experiences the constraint as near-beneficiary (d ≈ 0.15): it sets the rules, collects seigniorage, exempts itself from external discipline, and retains monetary autonomy. Non-reserve-currency-state seats experience it as near-target (d ≈ 0.82): they pay through policy subordination, reserve holding, and transmitted shocks, with minimal discretion to modify the peg. Peripheral economies (d ≈ 0.88) experience it as nearly full extraction: trapped by capital-account pressure, unable to pursue independent policy without immediate crisis, unable to exit without severe international consequences. The IMF seat (d ≈ 0.35) sits in a hybrid position: it administers the rules (beneficiary-adjacent) but is structurally subordinate to the reserve-currency issuer's veto power and enforces discipline it did not originate (payer-adjacent). Gold-standard-adherent states (d ≈ 0.58) sit nearest symmetric: they gain credibility from the gold commitment but lose autonomy when reserves drain; the U.S. gains exorbitant privilege precisely by inverting this trade-off (committing to gold convertibility in principle while retaining the option to default). Speculators (d ≈ 0.05) are excluded from decision-making but are the de facto enforcement agents; their exclusion from the rule set while their behavior enforces compliance is the mechanism by which peripheral states experience suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves a key mandatrophy tension: was Bretton Woods a genuine coordination mechanism whose founding mandate (prevent competitive devaluation, enable full employment with capital controls, stabilize trade) persisted, or did it become a snare whose founding mandate died while the extraction persisted? Under the 'sovereignty defense' reading, the mandate is CONTESTED: the founding problem (preventing competitive devaluation) remains live for the benefiting states (the U.S. benefits from a stable dollar peg as the anchor); but the founding problem (enabling national monetary autonomy) is dead for peripheral states by 1971 (autonomy is impossible under the peg, and the IMF's conditional loans require surrendering autonomy further). This is not pure mandatrophy because one beneficiary (the reserve-currency issuer) continues to benefit from the founding mandate's stability, while the payers (peripheral states) experience the mandate as obsolete while the constraint persists. The classification as tangled_rope captures this: genuine coordination function (trade stabilization, preventing devaluation races) AND asymmetric extraction (policy subordination, seigniorage transfer) are both structurally real. Neither is cover for the other — they coexist in the same constraint. The rising theater ratio (0.18→0.42) indicates growing theatricality as the coordination function becomes a smaller share of the total activity and more of the energy goes to maintaining the peg and enforcing discipline; but theater never reaches piton levels because the coordination function remains partially real and institutionally necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_symmetry,
    'Is the U.S. ability to finance deficits in its own currency a structural feature of the gold-exchange standard or a contingent policy choice the U.S. made within the system?',
    'Historical counterfactual: did the treaty text permit the U.S. to run persistent deficits, or did the U.S. choose to do so and force the system to accommodate? Compare drafting records (Keynes vs. White) against actual policy (Eisenhower administrations, Vietnam War spending).',
    'If structural, the constraint was designed to extract from the periphery; if contingent, the U.S. violated the treaty''s terms and the extraction is a breach, not a feature. Different classified types flow from each resolution: designed-extraction (snare) vs. treaty-violation (rope with false-flag beneficiary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_symmetry, empirical, 'Whether exorbitant privilege is intrinsic to Bretton Woods or a violation.').

omega_variable(
    reading_counterfactual_autonomy,
    'Under this reading''s own frame (sovereignty defense), would non-reserve states actually have gained monetary autonomy without the peg, or would they have faced worse external discipline (capital flight, competitive devaluation) under floating rates?',
    'Natural experiment: examine the monetary autonomy of states that exited or partially exited Bretton Woods (Britain 1976, France 1960s, development economists'' proposals for alternative blocs).',
    'If floating would have been worse, the constraint''s extraction enables a form of sovereignty (security against chaos). If floating would have been better, the constraint''s extraction purely subordinates. This modifies the ε interpretation without changing the measured value.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_counterfactual_autonomy, conceptual, 'Whether the constraint''s discipline is the price of sovereignty or the denial of it.').

omega_variable(
    reading_sibling_foreclosure,
    'Do the three Bretton Woods readings (sovereignty_defense, keynesian_embedded_liberalism, neoliberal_convertibility) represent logically incompatible claims about the same system, or different aspects of a single complex system that can all be true simultaneously?',
    'Formal analysis: do the core premises of each reading logically contradict each other (e.g., ''national autonomy preserved'' vs. ''national autonomy constrained'')? If so, foreclosure applies. If the readings dispute a factual question (does the system preserve autonomy? does it constrain capital?) rather than a logical one, they coexist.',
    'Determines the reading_relations classification: forecloses (mutual logical exclusivity) vs. coexists_with (factual dispute within a shared framework). Different impacts on how the corpus models kernel contests and reading decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure, conceptual, 'Whether Bretton Woods readings are logically incompatible or factually disputed.').

omega_variable(
    suppression_mechanism_structural_vs_ideological,
    'Is the measured suppression (0.71) structural (capital flight risk, balance-of-payments crisis mechanics) or ideological (belief in gold standard necessity, acceptance of external discipline as legitimate)?',
    'Post-exit observation: after Bretton Woods collapsed and the constraint ceased to bind, did the same states retain the suppressive beliefs and maintain internal discipline (ideological) or did they immediately pursue divergent policies (structural)?',
    'If ideological, the suppression might be internalized and persist after formal exit, requiring deeper institutional reform to overcome. If structural, removal of the external constraint suffices. Affects piton classification and theater interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_ideological, empirical, 'Whether suppression of monetary independence is mechanical or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_sovereign_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.18).
narrative_ontology:measurement(bw_sovereign_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(bw_sovereign_tr_t1956, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1956, 0.28).
narrative_ontology:measurement(bw_sovereign_tr_t1963, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1963, 0.35).
narrative_ontology:measurement(bw_sovereign_tr_t1968, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1968, 0.39).
narrative_ontology:measurement(bw_sovereign_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.42).

% Extraction over time
narrative_ontology:measurement(bw_sovereign_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.42).
narrative_ontology:measurement(bw_sovereign_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.51).
narrative_ontology:measurement(bw_sovereign_be_t1956, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1956, 0.58).
narrative_ontology:measurement(bw_sovereign_be_t1963, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1963, 0.63).
narrative_ontology:measurement(bw_sovereign_be_t1968, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1968, 0.66).
narrative_ontology:measurement(bw_sovereign_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bw_sovereign_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.48).
narrative_ontology:measurement(bw_sovereign_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.54).
narrative_ontology:measurement(bw_sovereign_su_t1956, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1956, 0.59).
narrative_ontology:measurement(bw_sovereign_su_t1963, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1963, 0.65).
narrative_ontology:measurement(bw_sovereign_su_t1968, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1968, 0.69).
narrative_ontology:measurement(bw_sovereign_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.18).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, international_monetary_fund_conditionality).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, triffin_dilemma_dollar_glut).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the contested Bretton Woods kernel. The kernel (bretton_woods_treaty_substrate) is interpreted by three distinct readings, each instantiating a different constraint with different ε values, beneficiary/victim sets, and classifications. The three readings are: (1) sovereignty_defense (this story) — emphasizes asymmetric external discipline imposed on non-reserve states; (2) keynesian_embedded_liberalism — emphasizes capital controls' protection of policy space; (3) neoliberal_convertibility — emphasizes constraints on monetary intervention. Each reading is a coherent constraint story with its own structural data. They are linked via network.affects_constraints to enable the corpus to model kernel contests and comparative reading analysis. Do NOT merge the three readings into one constraint with a 'measurement basis' or 'reading parameter' — each is a separate story with its own ε, claims, and metrics. The three stories together demonstrate the ε-invariance principle: the same treaty text (the kernel) is a different constraint under each reading because each reading identifies a different structural effect as primary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
