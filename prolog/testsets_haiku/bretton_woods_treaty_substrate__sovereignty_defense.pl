% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Monetary Discipline Asymmetry (Sovereignty-Defense Reading)
 *   domain: international_political_economy/monetary_institutions
 *
 * SUMMARY:
 *   The Bretton Woods treaty establishes a system of fixed exchange rates
 *   anchored to the U.S. dollar, which is convertible to gold at a fixed
 *   parity (35 USD per troy ounce). This constraint embeds a structural
 *   asymmetry: the United States, as the reserve-currency issuer, can finance
 *   deficits by issuing claims on itself (dollars) that other states
 *   accumulate as reserves. Non-reserve-currency states face external
 *   monetary discipline—they must maintain fixed rates and can only adjust
 *   through deflation, devaluation (which requires IMF approval), or reserve
 *   depletion. This SOVEREIGNTY-DEFENSE READING frames the constraint as a
 *   snare: the gold anchor and fixed-rate system appear to provide stability
 *   and preserve national monetary autonomy, but in practice they subordinate
 *   non-reserve states' monetary policy to external discipline while the U.S.
 *   extracts seigniorage. The other two readings of this kernel
 *   (keynesian_embedded_liberalism and neoliberal_convertibility) see the
 *   constraint differently—as protecting domestic policy space or enabling
 *   capital markets respectively—but this reading emphasizes the asymmetric
 *   extraction embedded in the institutional design.
 *
 * KEY AGENTS:
 *   - United States monetary authority: Issues the reserve currency and extracts exorbitant privilege through persistent balance-of-payments deficits financed by forced accumulation abroad.
 *   - Non-reserve-currency states (UK, France, West Germany): Face external discipline, constrained exit, and mandatory deflation to restore equilibrium.
 *   - Peripheral economies (developing countries): Depend on dollar credit but lack the ability to export their currency; face the steepest adjustment costs.
 *   - Bretton Woods Monetary Commission: Administers the system and enforces discipline asymmetrically (favoring the U.S. and its trading partners).
 *   - Speculative capital holders: Profit from dollar seigniorage and can threaten gold conversion if confidence erodes.
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
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, snare).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Monetary Discipline Asymmetry (Sovereignty-Defense Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_institutions").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '2f98d4b6-5b43-4ad5-8875-7ee6faa608ae').
narrative_ontology:cs_kernel_codification('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', formalized).
narrative_ontology:cs_authority_grounding('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', extraction).
narrative_ontology:cs_interpretation_layer_present('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae').
narrative_ontology:cs_reading_relation('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', bretton_woods_treaty_substrate__neoliberal_convertibility, coexists_with).
narrative_ontology:cs_axiom('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', foundational, fixed_rates_require_asymmetric_reserve_privilege).
narrative_ontology:cs_axiom_status(fixed_rates_require_asymmetric_reserve_privilege, holdable).
narrative_ontology:cs_axiom_grounding('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', fixed_rates_require_asymmetric_reserve_privilege, deontological).
narrative_ontology:cs_axiom('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', foundational, external_monetary_discipline_is_extraction_not_necessity).
narrative_ontology:cs_axiom_status(external_monetary_discipline_is_extraction_not_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', external_monetary_discipline_is_extraction_not_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', symmetric_monetary_sovereignty).
narrative_ontology:cs_drift_state('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', system_collapse_1971, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('2f98d4b6-5b43-4ad5-8875-7ee6faa608ae', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authority).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, trading_partners_of_united_states).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, speculative_capital_holders).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, fixed_exchange_rate_stability).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, gold_standard_credibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the dollar-gold parity (35 USD per troy ounce) and enforces it through the Federal Reserve's gold redemption commitment. Collects exorbitant privilege: the ability to run persistent balance-of-payments deficits denominated in its own currency, which other states must finance through dollar accumulation. Justifies the arrangement as providing a stable international monetary anchor for post-war reconstruction.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Must maintain fixed exchange rates against the dollar and hold substantial dollar reserves to back their currencies. Face external monetary discipline: if they run deficits or their reserves deplete, they must contract domestic demand, tighten credit, and deflate wages and prices to restore balance. Their exit is constrained: unilateral devaluation risks retaliation, capital flight, and loss of trade access; the constraint's enforcement is mutual surveillance and the threat of exclusion from dollar-denominated trade networks.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    powerful, generational, constrained, global).

% Depend on dollar credit for development finance and trade settlement. Face the same fixed-rate discipline as reserve-currency states but with far fewer exits: their currencies are not held internationally; their trade is priced in dollars; they have no ability to run deficits in their own currency. They receive the benefit of predictable exchange rates for trade, but the cost—mandatory internal deflation during external shortages—falls entirely on them because they cannot export their currency as a store of value.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies, beneficiary).

% Administers the par value system: approves currency adjustments, enforces Fund quotas, and coordinates surveillance of member states' external positions. Acts as both rule-enforcer and arbiter of permissible deviation, making decisions that advantage reserve-currency states and disadvantage peripheral economies in practice, while claiming neutral technical administration.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_monetary_commission, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_monetary_commission, observer).

% Gain from predictable dollar exchange rates and access to dollar credit for their own development. The constraint's benefits flow to them as cheap financing and stable trade terms; the costs of external discipline are borne by states outside the U.S.-allied trading bloc, creating a bifurcated system where alliance with the U.S. provides protection from the constraint.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, trading_partners_of_united_states, beneficiary,
    powerful, generational, constrained, global).

% Competing proposals for international monetary order (Keynes's International Clearing Union, multicurrency standard, regional currency unions) were rejected in favor of the dollar-gold system. Their exclusion is structural: the constraint's persistence depends on foreclosing debate about whether the arrangement could be organized differently while retaining its coordination function.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, excluded_alternative_monetary_architectures, excluded,
    analytical, generational, trapped, global).

% Hold dollar reserves at interest in the U.S. financial system, profiting from seigniorage and capital appreciation. Can exit the constraint by moving to alternative currencies or assets, but retain the option to demand gold conversion if confidence erodes, creating a structural vulnerability for the system that benefits them.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, speculative_capital_holders, beneficiary,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, united_states_monetary_authority).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes fixed exchange rates with a single numeraire (dollar pegged to gold) to eliminate exchange rate volatility, reduce transaction costs in international trade, and provide a credible anchor for price-level stability and credit expansion.
% TRANSFER_FUNCTION: Transfers monetary policy autonomy from non-reserve-currency states to the United States: the U.S. collects seigniorage (can run deficits financed by forced accumulation abroad); non-reserve states finance those deficits and accept external monetary discipline (fixed rates, required deflation, constrained credit policy).
% ABSENT_VOICES: Keynes and other architects of alternative proposals (International Clearing Union, multicurrency standards) whose designs for symmetric discipline were rejected; peripheral-economy policymakers (Latin America, Asia, Africa) who were not at Bretton Woods; labor movements in deficit-adjustment countries facing wage compression and unemployment; advocates for monetary pluralism and regional blocs.
% DISAPPEARANCE_RATIONALE: If the fixed-rate gold-dollar peg disappeared, the U.S. would immediately lose its capacity to finance deficits through forced accumulation; other states would adopt floating rates or regional arrangements; the international monetary system would reorganize around either decentralized competition or symmetric discipline (e.g., a new Clearing Union). The constraint's disappearance in 1971 produced exactly this outcome: floating rates, petrodollars, regional blocs, and the eventual need for explicit recycling of surpluses.
% FOUNDING_PROBLEM: In the 1930s–1940s, competitive devaluation, capital flight, and lack of a shared numeraire for international settlement created trade collapse and beggar-thy-neighbor policies. Bretton Woods was designed to restore multilateral trade by fixing exchange rates and providing sufficient credit for reconstruction.
% FOUNDING_PROBLEM_CORROBORATION: The historical problem is attested by economic historians (Eichengreen, Kindleberger, Temin) analyzing the 1930s collapse. By the 1960s, the founding problem status becomes contested: the benefiting parties (U.S. Treasury, Federal Reserve) claim it remains live; the victim parties (UK, France, developing countries) and independent scholars (Triffin, Haberler, later Steil) attest the problem is solved and the constraint persists as extraction, not necessity. The 1971 collapse occurred not because the founding problem re-emerged, but because the asymmetry became unsustainable—confirming that the problem status had shifted from live to dead, and the constraint's function had shifted from coordination to extraction.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).

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
 *   Extractiveness rises from 0.38 (1944, system inception) to 0.68 (1971, collapse), tracking the growing divergence between the U.S. monetary autonomy and the external discipline faced by other states. In 1944, the constraint is presented as a mechanism for stability; by the 1960s, the U.S. is running persistent deficits while other states absorb the adjustment burden. Suppression requirement rises from 0.42 to 0.71, reflecting increasing enforcement intensity: the Bretton Woods Commission, the IMF, bilateral negotiations, and the credible threat of trade retaliation all work to enforce the discipline. Theater ratio rises from 0.15 to 0.42, reflecting the growing disjunction between the stated function (stability) and the actual function (subsidizing U.S. deficits). Accessibility_collapse is high across all levels because the fixed-rate system was nearly universal for trade and finance; alternatives (flexible rates, regional blocs) were actively foreclosed. Resistance grows from the mid-1950s onward as non-reserve states recognize the asymmetry and seek escape routes (Suez crisis 1956, French withdrawal of gold 1965, Triffin debates, ultimately Nixon's 1971 unilateral exit). The coercion grid shows class-level resistance (developing countries and European labor movements) accelerating faster than organizational or structural resistance, because the burden of adjustment fell most heavily on wage-earners and import-competing sectors in peripheral economies.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. Treasury and Federal Reserve perceive the constraint as a legitimate arrangement for monetary stability, justified by their provision of the reserve currency and the public good of stable exchange rates. They experience d near 0.0 (beneficiary). Non-reserve-currency states perceive the same arrangement as external discipline that subordinates their monetary autonomy; they experience d near 1.0 (target). Peripheral economies experience the constraint even more severely because they lack the sophistication, credit access, and political leverage of the major industrial states; their d approaches 1.0 on the identity-locked axis (they have no currency alternative for international settlement). The engine computes these differences from the structural data: beneficiary vs. victim declarations, power atoms, exit options. The claimed type (snare) reflects the sovereignty-defense reading's structural position; the metrics (high extractiveness, high suppression) describe the asymmetric operation the beneficiary states would deny but peripheral states would attest.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States is declared as beneficiary because it collects seigniorage (issues the reserve currency) and runs persistent deficits financed by forced accumulation abroad; its directionality is d ≈ 0.15 (near-beneficiary but not pure coordination—the benefit rides on suppression of alternatives). Non-reserve-currency states and peripheral economies are declared as victims because they face imported discipline and constrained exit; their directionality is d ≈ 0.82–0.95 (near-target, with peripheral economies identity-locked). The major industrial states outside the U.S. (UK, France, West Germany) sit higher on the power axis than peripheral economies, which gives them better exit options (bilateral negotiations, eventually the option to float unilaterally), but the constraint still targets them with external discipline. Peripheral economies have no such exit and are fully identity-locked into the dollar system for international settlement. Speculative capital holders benefit from dollar seigniorage but retain arbitrage (mobile exit), placing them d ≈ 0.25–0.40. The IMF Commission occupies an agenda-setter role but is captured by the interests of the U.S. and its allies, so it derives d from the U.S. position, not from a neutral technical function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem—exchange rate instability and competitive devaluation in the 1930s–1940s—was genuinely acute and the fixed-rate gold anchor provided real coordination benefits in the immediate post-war period (reconstruction, trade growth). By the 1960s, the founding problem status becomes contested: economic recovery is largely complete; the constraint's persistence depends increasingly on extraction (U.S. seigniorage collection and the subjugation of other states' monetary policy) rather than the coordination function. The constraint is classified as snare, not rope, because the measured extraction and suppression exceed what coordination alone requires, and because alternatives (floating rates, regional blocs, symmetric discipline) were actively foreclosed despite their growing viability. The theater ratio's rise (0.15 to 0.42) documents the mounting disjunction between stated function and actual function. The constraint resolves mandatrophy toward snare: it persists because the U.S. benefits from it and has the power to defend it, not because the founding coordination problem remains live. The 1971 collapse (when Nixon unilaterally ended gold convertibility) confirms this reading: the constraint vanished not because the coordination problem re-emerged, but because the extractive burden became unsustainable and the U.S. chose to exit rather than reduce its deficits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exorbitant_privilege_necessity,
    'Was the U.S. exorbitant privilege—the ability to run persistent deficits financed by forced accumulation abroad—structurally necessary for post-war reconstruction and growth, or was it a side effect of choosing the dollar as numeraire that could have been constrained by symmetric discipline?',
    'Comparison with alternative arrangements (Keynes''s Clearing Union would have required symmetric adjustment; multicurrency or gold-based standards would have limited U.S. deficit financing). Historical counterfactuals from economists (Steil, Eichengreen, Helleiner) comparing actual growth outcomes to simulated outcomes under alternative rules.',
    'If symmetric discipline was feasible, the U.S. exorbitant privilege is pure extraction, supporting the snare classification. If only asymmetric discipline could have provided enough credit for reconstruction, the extraction is a necessary cost of the coordination function, moving the constraint toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_necessity, conceptual, 'Whether U.S. deficits were structurally necessary or institutionally chosen.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Did non-reserve-currency states accept the external discipline because they internalized the doctrine that fixed rates and balanced budgets were economically correct (''true'' policy), or because structural conditions (capital dependence, trade vulnerability, lack of alternative credit) trapped them into compliance regardless of belief?',
    'Archival evidence of policy debate in non-reserve states (did elites contest the discipline on principle, or only on feasibility?); post-exit behavior after 1971 (did states maintain discipline voluntarily, or change course immediately?). Political economy analysis of coalition dynamics within non-reserve states (labor, importers, exporters).',
    'If internalized: the suppression score reflects genuine belief in the constraint''s legitimacy, and peripheral economies would maintain discipline post-exit. If structural: the suppression score reflects coercion, and the constraint should be reclassified toward pure snare (higher suppression, lower theater). Post-1971 floating-rate adoption and rapid monetary expansion in Europe and Japan support the structural (trapped) interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether external discipline was accepted as doctrine or imposed by structural dependence.').

omega_variable(
    collective_action_asymmetry,
    'Could non-reserve-currency states have collectively threatened to abandon the system (coordinating a run on the dollar or unified devaluation) and forced more symmetric discipline, or was collective action prevented by coordination problems and U.S. power?',
    'Game-theoretic analysis of the coordination problem (each state''s individual incentive to maintain access to dollar credit vs. collective interest in reforming rates). Historical evidence of attempted coordination (IMF negotiations, regional currency proposals, bilateral talks) and the barriers that prevented successful renegotiation until the system collapsed unilaterally.',
    'If collective action was feasible, the victims bear some responsibility for the extraction, and the suppression score should reflect coordination failure rather than pure coercion. If collective action was structurally prevented, the suppression is exogenous and the snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_asymmetry, empirical, 'Whether the victims had a realistic collective exit option that was foreclosed by coordination problems.').

omega_variable(
    kernel_reading_alternative_framings,
    'Within the sovereignty-defense reading itself, is the constraint better classified as a snare (extractive, persisting through suppression of alternatives) or as a tangled_rope (coordination function + asymmetric extraction, both necessary to the same structure)?',
    'Decompose the constraint into its coordination component (fixed rates enable trade settlement, reduce transaction costs) and its extraction component (U.S. runs deficits, other states accumulate dollars). If the coordination function could be separated from the extraction (e.g., fixed rates could operate with symmetric discipline), the components are separable and the constraint is snare. If fixed rates inherently require a reserve-currency issuer with asymmetric privileges, the components are inseparable and the constraint is tangled_rope.',
    'Snare classification emphasizes the foreclosure of alternatives; tangled_rope emphasizes the genuine coordination benefit alongside extraction. The coercion_grid and measurement series support snare (rising theater ratio, rising extraction without corresponding coordination benefit). But if scholars in the embedded-liberalism reading can identify genuine coordination value that persisted, a tangled_rope interpretation becomes defensible for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_alternative_framings, conceptual, 'Whether the constraint''s coordination and extraction components are inseparable or could be disaggregated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.15).
narrative_ontology:measurement_basis(bret_tr_t1944, observed).
narrative_ontology:measurement(bret_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.22).
narrative_ontology:measurement_basis(bret_tr_t1950, observed).
narrative_ontology:measurement(bret_tr_t1956, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1956, 0.28).
narrative_ontology:measurement_basis(bret_tr_t1956, observed).
narrative_ontology:measurement(bret_tr_t1963, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1963, 0.37).
narrative_ontology:measurement_basis(bret_tr_t1963, observed).
narrative_ontology:measurement(bret_tr_t1968, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1968, 0.42).
narrative_ontology:measurement_basis(bret_tr_t1968, observed).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.42).
narrative_ontology:measurement_basis(bret_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.38).
narrative_ontology:measurement_basis(bret_be_t1944, observed).
narrative_ontology:measurement(bret_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.47).
narrative_ontology:measurement_basis(bret_be_t1950, observed).
narrative_ontology:measurement(bret_be_t1956, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1956, 0.55).
narrative_ontology:measurement_basis(bret_be_t1956, observed).
narrative_ontology:measurement(bret_be_t1963, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1963, 0.64).
narrative_ontology:measurement_basis(bret_be_t1963, observed).
narrative_ontology:measurement(bret_be_t1968, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1968, 0.68).
narrative_ontology:measurement_basis(bret_be_t1968, observed).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(bret_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.42).
narrative_ontology:measurement_basis(bret_su_t1944, observed).
narrative_ontology:measurement(bret_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.53).
narrative_ontology:measurement_basis(bret_su_t1950, observed).
narrative_ontology:measurement(bret_su_t1956, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1956, 0.61).
narrative_ontology:measurement_basis(bret_su_t1956, observed).
narrative_ontology:measurement(bret_su_t1963, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1963, 0.68).
narrative_ontology:measurement_basis(bret_su_t1963, observed).
narrative_ontology:measurement(bret_su_t1968, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1968, 0.71).
narrative_ontology:measurement_basis(bret_su_t1968, observed).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.71).
narrative_ontology:measurement_basis(bret_su_t1971, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1944, tn=1971
narrative_ontology:measurement(bret_grid_01, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(class), 1944, 0.64).
narrative_ontology:measurement(bret_grid_02, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(class), 1971, 0.78).
narrative_ontology:measurement(bret_grid_03, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(individual), 1944, 0.42).
narrative_ontology:measurement(bret_grid_04, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(individual), 1971, 0.51).
narrative_ontology:measurement(bret_grid_05, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(organizational), 1944, 0.58).
narrative_ontology:measurement(bret_grid_06, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(organizational), 1971, 0.72).
narrative_ontology:measurement(bret_grid_07, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(structural), 1944, 0.72).
narrative_ontology:measurement(bret_grid_08, bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse(structural), 1971, 0.81).
narrative_ontology:measurement(bret_grid_09, bretton_woods_treaty_substrate__sovereignty_defense, resistance(class), 1944, 0.41).
narrative_ontology:measurement(bret_grid_10, bretton_woods_treaty_substrate__sovereignty_defense, resistance(class), 1971, 0.68).
narrative_ontology:measurement(bret_grid_11, bretton_woods_treaty_substrate__sovereignty_defense, resistance(individual), 1944, 0.32).
narrative_ontology:measurement(bret_grid_12, bretton_woods_treaty_substrate__sovereignty_defense, resistance(individual), 1971, 0.51).
narrative_ontology:measurement(bret_grid_13, bretton_woods_treaty_substrate__sovereignty_defense, resistance(organizational), 1944, 0.28).
narrative_ontology:measurement(bret_grid_14, bretton_woods_treaty_substrate__sovereignty_defense, resistance(organizational), 1971, 0.63).
narrative_ontology:measurement(bret_grid_15, bretton_woods_treaty_substrate__sovereignty_defense, resistance(structural), 1944, 0.34).
narrative_ontology:measurement(bret_grid_16, bretton_woods_treaty_substrate__sovereignty_defense, resistance(structural), 1971, 0.62).
narrative_ontology:measurement(bret_grid_17, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(class), 1944, 0.47).
narrative_ontology:measurement(bret_grid_18, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(class), 1971, 0.68).
narrative_ontology:measurement(bret_grid_19, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(individual), 1944, 0.38).
narrative_ontology:measurement(bret_grid_20, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(individual), 1971, 0.54).
narrative_ontology:measurement(bret_grid_21, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(organizational), 1944, 0.52).
narrative_ontology:measurement(bret_grid_22, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(organizational), 1971, 0.71).
narrative_ontology:measurement(bret_grid_23, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(structural), 1944, 0.61).
narrative_ontology:measurement(bret_grid_24, bretton_woods_treaty_substrate__sovereignty_defense, stakes_inflation(structural), 1971, 0.74).
narrative_ontology:measurement(bret_grid_25, bretton_woods_treaty_substrate__sovereignty_defense, suppression(class), 1944, 0.41).
narrative_ontology:measurement(bret_grid_26, bretton_woods_treaty_substrate__sovereignty_defense, suppression(class), 1971, 0.74).
narrative_ontology:measurement(bret_grid_27, bretton_woods_treaty_substrate__sovereignty_defense, suppression(individual), 1944, 0.35).
narrative_ontology:measurement(bret_grid_28, bretton_woods_treaty_substrate__sovereignty_defense, suppression(individual), 1971, 0.58).
narrative_ontology:measurement(bret_grid_29, bretton_woods_treaty_substrate__sovereignty_defense, suppression(organizational), 1944, 0.48).
narrative_ontology:measurement(bret_grid_30, bretton_woods_treaty_substrate__sovereignty_defense, suppression(organizational), 1971, 0.72).
narrative_ontology:measurement(bret_grid_31, bretton_woods_treaty_substrate__sovereignty_defense, suppression(structural), 1944, 0.54).
narrative_ontology:measurement(bret_grid_32, bretton_woods_treaty_substrate__sovereignty_defense, suppression(structural), 1971, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.18).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% The Bretton Woods treaty substrate is a contested kernel with three structurally distinct readings. The sovereignty-defense reading emphasizes asymmetric extraction and the subordination of non-reserve-currency states' monetary autonomy. The keynesian_embedded_liberalism reading emphasizes the same institutions as protecting domestic policy space and capital controls. The neoliberal_convertibility reading emphasizes the same institutions as enabling free capital markets. All three readings operate on the same formal rule set (fixed parities, gold peg, IMF governance) but derive different ε values (extraction scores) from different interpretations of which functions the constraint serves. This reading (sovereignty-defense) has ε ≈ 0.68 (snare classification); the embedded-liberalism reading should derive ε ≈ 0.35–0.45 (rope classification, emphasis on coordination); the neoliberal reading should derive ε ≈ 0.50–0.60 (tangled_rope classification, coordination + capital market efficiency extraction). The network links allow cross-reading analysis: does the corpus classify the same institutions differently when the authoring seat changes? The divergence is the measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__sovereignty_defense, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
