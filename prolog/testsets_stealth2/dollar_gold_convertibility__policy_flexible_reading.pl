% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Obligation Subordinate to Domestic Economic Stability (Policy-Flexible Reading)
 *   domain: economic/political/legal
 *
 * SUMMARY:
 *   Under the policy-flexible reading, the Bretton Woods Article IV
 *   undertaking — the U.S. commitment to buy and sell gold from monetary
 *   authorities at thirty-five dollars per ounce — is a conditional
 *   obligation that yields to domestic economic stability when the two
 *   conflict. The standing arrangement this story is about: the U.S. supplies
 *   the world's reserve asset while reserving the option to suspend, and
 *   external creditors hold balances whose gold value rests on U.S.
 *   discretion. Over the interval the arrangement migrates from generous
 *   liquidity provision (Marshall-era dollar scarcity) through crisis
 *   management (Gold Pool, swap lines, offset agreements) to unilateral
 *   termination (August 1971), with the adjustment burden shifting
 *   progressively onto external creditors. This file is ONE READING of the
 *   dollar_gold_convertibility kernel; the strict_convertibility_reading and
 *   triffin_structural_reading are separate constraints with their own
 *   epsilon values, victim sets, and classifications, linked through
 *   network.affects_constraints. The claimed type (tangled_rope) and the
 *   authored metrics are independent facts: the metrics describe an
 *   arrangement whose coordination function is real but whose risk allocation
 *   becomes sharply asymmetric late in its life.
 *
 * KEY AGENTS:
 *   - united_states_monetary_authorities: agenda-setter and primary beneficiary (institutional/arbitrage) — administers the gold window, collects seigniorage, holds and finally exercises the suspension option
 *   - united_states_fiscal_authority: secondary beneficiary (powerful/arbitrage) — deficit financing absorbed externally, no binding external check
 *   - foreign_monetary_authorities: primary target (organized/trapped) — hold the devaluation risk; the conversion right is politically neutralized by collective-action binds and diplomatic linkage
 *   - private_foreign_dollar_holders: target (moderate/constrained) — bear devaluation risk with no governance voice; exit priced out after 1968
 *   - surplus_export_economies: dual-positioned (organized/constrained) — coordination beneficiaries through export-led growth, forced accumulators of unwanted dollar reserves
 *   - french_gold_convertibility_camp: resisting target (organized/constrained) — exercises the conversion right at escalating political cost
 *   - imf_par_value_administrator: analytical observer (institutional/analytical) — documents the dollar-glut arithmetic, brokers the SDR
 *   - gold_producing_economies: excluded voice (moderate/constrained) — revenue capped by the official price, no seat in governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.55).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Obligation Subordinate to Domestic Economic Stability (Policy-Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "economic/political/legal").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'd31d0d44-d5a1-4b55-b35a-98629d8a3d54').
narrative_ontology:cs_kernel_codification('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', fixed_text).
narrative_ontology:cs_authority_grounding('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', practice).
narrative_ontology:cs_interpretation_layer_present('d31d0d44-d5a1-4b55-b35a-98629d8a3d54').
narrative_ontology:cs_reading_relation('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', dollar_gold_convertibility__strict_convertibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', dollar_gold_convertibility__triffin_structural_reading, influences).
narrative_ontology:cs_axiom('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', foundational, domestic_stability_trumps_external_commitment).
narrative_ontology:cs_axiom_status(domestic_stability_trumps_external_commitment, holdable).
narrative_ontology:cs_axiom_grounding('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', domestic_stability_trumps_external_commitment, instrumental).
narrative_ontology:cs_axiom('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', secondary, declared_conditionality_preserves_credibility).
narrative_ontology:cs_axiom_status(declared_conditionality_preserves_credibility, holdable).
narrative_ontology:cs_axiom_grounding('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', declared_conditionality_preserves_credibility, conventional).
narrative_ontology:cs_reference_frame('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', conditional_convertibility_policy_primacy).
narrative_ontology:cs_drift_state('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', nixon_shock_moment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d31d0d44-d5a1-4b55-b35a-98629d8a3d54', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, united_states_fiscal_authority).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, surplus_export_economies).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_monetary_authorities).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, private_foreign_dollar_holders).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, surplus_export_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, french_gold_convertibility_camp).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, conditional_obligation_doctrine).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, domestic_stability_supremacy_principle).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, managed_flexibility_preserves_open_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the gold window at thirty-five dollars per ounce, deciding daily which conversion requests to honor and on what terms; chairs the London Gold Pool, extends swap lines to friendly central banks, and applies moral suasion, capital controls, and troop-offset agreements to slow the gold drain. Collects seigniorage on the world's reserve balances and retains, and ultimately exercises, the authority to suspend conversion when domestic objectives require it.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities, beneficiary).

% Finances overseas military commitments and domestic programs partly through balance-of-payments deficits settled in dollars. Benefits from external absorption of the resulting balances and from the absence of a binding external check on budgetary and monetary choices; sets domestic policy first and adjusts the external commitment to fit, as the 1971 suspension demonstrates.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, united_states_fiscal_authority, beneficiary,
    powerful, biographical, arbitrage, national).

% Hold growing dollar balances as their principal reserve asset under par-value commitments of their own. Formally entitled to present dollars for gold, they face a collective bind: large-scale presentation would exhaust the U.S. gold stock, collapse the parity system, and destroy the value of their own remaining balances. Diplomatic linkage — troop offsets, swap-conditioned restraint — discourages exercise of the conversion right.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_monetary_authorities, payer,
    organized, generational, trapped, global).

% Firms, banks, and individuals outside the United States holding dollar deposits and claims. Before 1968 they could buy gold in the London market near the official price; afterward the two-tier market priced private buyers out. Diversification into other currencies was limited by shallow markets and home-country exchange controls. Shifting funds ahead of devaluation expectations was their main lever, and they used it repeatedly.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, private_foreign_dollar_holders, payer,
    moderate, immediate, constrained, global).

% Run persistent trade surpluses inside undervalued parities, gaining export-led growth and reserve accumulation. The same surpluses compel them to accumulate dollars they did not choose to hold, importing U.S. inflation and carrying devaluation risk on their reserves. Revaluation would protect reserves but sacrifice competitiveness, so they lobby for U.S. adjustment instead and resist revaluation until 1969-71.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, surplus_export_economies, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, surplus_export_economies, payer).

% A policy faction around the presidency and its economic advisers that treats the metallic anchor as the system's only honest discipline. Converts dollar reserves into gold at scale through the 1960s, publishes the case against deficit-financed privilege, and presses for a gold-rule regime. Its conversions draw diplomatic retaliation and strain alliance ties, capping how far the exit can be pushed.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, french_gold_convertibility_camp, payer,
    organized, biographical, constrained, continental).

% Administers par values, reviews members' exchange practices, and brokers standby arrangements. Its formal powers over the largest member are advisory; it documents the shift from dollar scarcity to dollar glut and hosts the negotiation that produces the Special Drawing Right in 1969 as a substitute reserve asset.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, imf_par_value_administrator, observer,
    institutional, generational, analytical, global).

% Suppliers of newly mined gold whose receipts depend on the official thirty-five-dollar price. Excluded from the regime's governance, they would press for a higher parity that raises their revenue; the fixed price, and after 1968 the two-tier market, caps their returns. They appear in the arrangement only as sellers into the pool and as background pressure on the price.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, gold_producing_economies, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, united_states_monetary_authorities).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single anchor for the postwar payments system: fixed par values, a universally acceptable reserve asset backed by a gold undertaking, liquidity for rapidly expanding trade, and reconstruction finance while Europe and Japan rebuild.
% TRANSFER_FUNCTION: Moves real goods, services, and assets from the rest of the world to the United States, and dollar-denominated credit outward, in exchange for dollar balances whose gold value depends on continued U.S. willingness to convert; when conversion stops, the outstanding balances absorb the loss.
% ABSENT_VOICES: Gold-producing economies excluded from governance would object to the suppressed official price; future holders of the post-1971 dollar overhang had no seat; domestic U.S. constituencies that would bear any disciplined adjustment were represented only rhetorically. Dissent existed inside the regime mainly as French diplomatic protest and market speculation, not as seated voice.
% DISAPPEARANCE_RATIONALE: Overnight disappearance forces every member to choose immediately among floating, gold revaluation, import rationing, or bilateral clearing; trade finance contracts to cash-in-advance; the reserve systems of dozens of countries lose their principal asset. The entire postwar payments architecture is organized around this undertaking.
% FOUNDING_PROBLEM: Interwar monetary chaos: competitive devaluations, beggar-thy-neighbor tariffs, the collapse of the classical gold standard, and discriminatory bilateral clearing. Bretton Woods was built to combine gold-anchored credibility with national autonomy for employment policy.
% FOUNDING_PROBLEM_CORROBORATION: The 1944 conference record and the contemporaneous White and Keynes drafts attest the founding problem from outside any benefiting party. By the 1960s, Rueff's and Triffin's published critiques — authors outside the U.S. beneficiary set — attest that the original problem had been largely solved and transformed into a dollar-glut problem; IMF annual reports document the same shift from dollar scarcity to dollar overhang.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the risk allocation this reading makes visible: the U.S. finances deficits with paper whose redemption depends on its own discretion, and the 1971 suspension transferred the accumulated loss to balance-holders. Suppression (0.55) is structural, not internalized: capital controls, the Interest Equalization Tax, two-tier gold pricing, and diplomatic linkage raised the cost of exercising formal rights; it is authored unscaled, as a raw property of the arrangement. Theater (0.58) rises because declaratory defense of the parity decoupled from actual gold flows — solemn reaffirmations issued months before suspension, the Gold Pool announcing defense while rationing, the SDR presented as system-strengthening while functioning as gold-substitution. Accessibility_collapse (0.42) is moderate: exits existed (conversion, gold-market purchase, currency diversification, the SDR) but each was priced, rationed, or policed rather than eliminated. Resistance (0.62) is high and sustained: the French conversion campaign, Bundesbank reluctance, repeated speculative attacks, and the 1968 pool collapse. The three measurement series share one eight-point grid spanning 1945-1971; the suppression_requirement series is authored deliberately because the story traces an enforcement-capacity arc — buildup through the 1960s, then decay at the endpoint as the U.S. chose exit over further enforcement. Creditor-coalition potential was real (G10, Gold Pool) but dissolved through free-riding, which is why individually trapped seats stayed trapped.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the U.S. monetary seat the arrangement is managed flexibility preserving an open trading order — the conditionality is a safety valve that keeps the system honest rather than a reserved loophole. From the trapped creditor seats the same structure operates as unilateral risk transfer: formal entitlement to gold, practical inability to exercise it. European legal advisers holding the strict reading experience the identical treaty clause as binding law constraining U.S. policy; the divergence between their reading and this one is a fact about different parties' commitments, computed from the structural data rather than adjudicated by the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. seats sit near the beneficiary pole: they collect seigniorage and deficit financing, and their arbitrage-grade exit (they set the terms and can rewrite them) pushes effective extraction away from them — toward subsidy, in effect. Foreign monetary authorities sit near the full-target pole, amplified by trapped exit: their wealth is denominated in the very claim whose redemption the agenda-setter controls. Private foreign holders are similarly targeted with constrained exit. Surplus export economies carry dual declarations and land mid-range: genuine coordination gains (export-led growth inside undervalued parities) against forced accumulation losses. The French camp is targeted despite actively exercising exit, because their exit is politically capped — each conversion purchased at alliance cost. Global spatial scope modestly amplifies effective extraction for the targeted seats (verification of the U.S. commitment is harder at planetary scale); the engine owns that scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar disorder — was substantially solved by the late 1950s, yet the arrangement persisted and acquired new functions: U.S. deficit finance and export-led growth for the surplus economies. Status is therefore contested rather than dead, and the mismatch consumer reads contested x world_rearranges, asserting no zombie flag. The classification guards against two mislabels: not a snare, because the coordination function is genuine (liquidity provision, reconstruction finance, fixed-rate predictability demonstrably supported the postwar expansion); not a rope, because the risk allocation is asymmetric and held in place by active enforcement — suasion, controls, rationing — not by participant consent alone. The piton test fails cleanly: a concentrated beneficiary (the U.S. monetary-fiscal complex) both captures the gains and administers the arrangement, so gain_flow names a seat rather than running diffuse, and the administrator's cost of fixing (domestic austerity or multilateral revision) exceeded what it bore — hence fixing_cost prohibitive. Mandatrophy here is transformation, not death: the mandate's object shifted from rebuilding openness to financing U.S. external deficits while the original label remained attached.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint instantiates the policy_flexible_reading of kernel dollar_gold_convertibility; how would the classification change under the strict_convertibility_reading, under which the Article IV undertaking binds regardless of domestic conditions?',
    'Comparative compile of the sibling story: under the strict reading the U.S. enters the paying set (its policy autonomy is what is taken), dollar holders leave the victim set, and effective extraction redistributes from external creditors toward the U.S. domestic-policy seat.',
    'Victim and beneficiary sets invert between readings; the same historical episodes support opposite classifications depending on which reading''s obligation structure is compiled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-contingent victim set and extraction direction within the convertibility kernel.').

omega_variable(
    discretion_or_triffin_inevitability,
    'Was the conditionality a real discretionary option the U.S. held and chose to exercise, or the inevitable working-out of the Triffin arithmetic dressed as policy choice?',
    'Counterfactual analysis of the adjustment episodes (1960, 1965, 1968, 1971): whether any feasible U.S. policy mix — taxation, credit restraint, gold-price action — could have restored external balance while honoring conversion; archival decision records on whether suspension was ever treated as avoidable.',
    'If inevitability, the arrangement behaved as transitional support whose end was designed-in, the flexibility claim collapses into the triffin_structural_reading''s story, and this reading''s discretion-based extraction attribution weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_or_triffin_inevitability, empirical, 'Whether flexibility was genuine option space or structural determinism.').

omega_variable(
    suasion_binding_force,
    'How much of the measured suppression reflects binding coercion on foreign authorities versus cooperation they would have extended voluntarily for their own stabilization interests?',
    'Archival study of the offset and swap negotiations (Germany 1966-67, Japan late 1960s): classify decisions taken under explicit linkage threats versus convergent interest.',
    'If mostly voluntary, suppression is overstated and the arrangement sits closer to a rope with asymmetric side-payments; if coerced, the tangled_rope reading with strong enforcement dependence is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suasion_binding_force, empirical, 'Coercive versus cooperative share of the enforcement machinery.').

omega_variable(
    creditor_net_position_ambiguity,
    'Were the surplus export economies net payers or net beneficiaries once export-led growth gains are weighed against forced accumulation and devaluation losses?',
    'Welfare accounting of the German and Japanese positions 1958-1971: reserve losses valued at post-1971 prices versus cumulative export gains attributable to the parity structure.',
    'If net beneficiaries, their directionality drops toward the beneficiary pole, the victim set narrows to private holders and deficit-country authorities, and the measured extraction concentrates more sharply on the remaining targeted seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creditor_net_position_ambiguity, preference, 'Net-position ambiguity of the dual-positioned surplus seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t0, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t5, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t5, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t10, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t10, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t13, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 13, 0.32).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t13, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t15, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t15, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t20, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t20, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t23, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 23, 0.52).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t23, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_tr_t26, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 26, 0.58).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t0, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t5, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 5, 0.36).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t5, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t10, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t10, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t13, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 13, 0.5).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t13, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t15, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t15, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t20, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t20, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t23, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 23, 0.64).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t23, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_be_t26, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 26, 0.68).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t0, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t0, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t5, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t5, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t10, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t10, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t13, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 13, 0.5).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t13, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t15, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 15, 0.56).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t15, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t20, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t20, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t23, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 23, 0.67).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t23, observed).
narrative_ontology:measurement(dollar_gold_policy_flexible_su_t26, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 26, 0.55).
narrative_ontology:measurement_basis(dollar_gold_policy_flexible_su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Bretton Woods convertibility' decomposes into three structurally distinct claims with different epsilon, victim sets, and failure modes — a bindingness dispute (strict vs. policy-flexible readings of the same Article IV text) and a sustainability dispute (the Triffin reading, under which no reading of the obligation survives the arithmetic). Upstream/downstream structure: the policy-flexible reading's tolerated deficits generated the dollar overhang that supplies the evidentiary base the Triffin reading cites, so this story influences the triffin story's operating environment without foreclosing it; the strict and flexible readings coexist as live positions held by different parties (European discipline advocates vs. U.S. officials) with neither eliminable within a single party's framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
