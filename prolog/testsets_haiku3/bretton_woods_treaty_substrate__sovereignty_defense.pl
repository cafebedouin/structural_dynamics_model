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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods Fixed Exchange Regime as Sovereignty Defense Constraint
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   Bretton Woods (1944–1971) fixed exchange rates to gold via the U.S.
 *   dollar at $35/oz, constrained international capital flows, and created an
 *   IMF surveillance regime. This reading interprets the regime as defending
 *   national monetary sovereignty against external discipline by anchoring to
 *   gold—a substitute for unilateral currency control. However, the
 *   sovereignty defense is asymmetric: the U.S. could issue reserve currency
 *   and adjust unilaterally; non-reserve-currency states were locked into
 *   fixed parities and had to defend their currencies through deflation or
 *   IMF-negotiated devaluation. The constraint combines genuine coordination
 *   (eliminating 1930s chaos) with systematic extraction (seigniorage
 *   asymmetry, subordination of domestic policy to external balance). The
 *   claimed type is tangled_rope: both functions are present, but the
 *   extraction function intensified over time as the coordination problem
 *   weakened. The measurement series tracks this divergence: extractiveness
 *   and theater_ratio both rise while suppression_requirement plateaus,
 *   signaling that the regime's enforcement machinery shifted from
 *   coordination to extraction defense.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (U.S.) — sets parities, collects seigniorage, can adjust unilaterally
 *   - non_reserve_currency_states (Britain, Europe, Japan, etc.) — locked into fixed rates, bear adjustment burden
 *   - peripheral_economies (commodity exporters, developing nations) — trapped in subordinate positions, subject to IMF conditionality
 *   - IMF governance board — enforces discipline and exchange-rate defense requirements
 *   - labor movements and Keynesians — excluded from policy design, bear costs of exchange-rate overvaluation and demand-side subordination
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
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods Fixed Exchange Regime as Sovereignty Defense Constraint").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, 'a81c698f-208f-4438-ba5e-0356a2ac14bf').
narrative_ontology:cs_kernel_codification('a81c698f-208f-4438-ba5e-0356a2ac14bf', formalized).
narrative_ontology:cs_authority_grounding('a81c698f-208f-4438-ba5e-0356a2ac14bf', extraction).
narrative_ontology:cs_interpretation_layer_present('a81c698f-208f-4438-ba5e-0356a2ac14bf').
narrative_ontology:cs_reading_relation('a81c698f-208f-4438-ba5e-0356a2ac14bf', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('a81c698f-208f-4438-ba5e-0356a2ac14bf', bretton_woods_treaty_substrate__neoliberal_convertibility, influences).
narrative_ontology:cs_axiom('a81c698f-208f-4438-ba5e-0356a2ac14bf', foundational, national_monetary_sovereignty_requires_gold_anchor).
narrative_ontology:cs_axiom_status(national_monetary_sovereignty_requires_gold_anchor, holdable).
narrative_ontology:cs_axiom_grounding('a81c698f-208f-4438-ba5e-0356a2ac14bf', national_monetary_sovereignty_requires_gold_anchor, conventional).
narrative_ontology:cs_axiom('a81c698f-208f-4438-ba5e-0356a2ac14bf', secondary, fixed_exchange_rates_protect_against_external_monetary_cycles).
narrative_ontology:cs_axiom_status(fixed_exchange_rates_protect_against_external_monetary_cycles, overridden).
narrative_ontology:cs_axiom_grounding('a81c698f-208f-4438-ba5e-0356a2ac14bf', fixed_exchange_rates_protect_against_external_monetary_cycles, empirically_contingent).
narrative_ontology:cs_reference_frame('a81c698f-208f-4438-ba5e-0356a2ac14bf', gold_standard_monetary_anchor).
narrative_ontology:cs_drift_state('a81c698f-208f-4438-ba5e-0356a2ac14bf', post_1960_triffin_dilemma_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('a81c698f-208f-4438-ba5e-0356a2ac14bf', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuer).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, capital_exporting_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, labor_importing_states).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, state_monetary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__sovereignty_defense, fixed_exchange_stabilization_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The United States, which sets and enforces the dollar-gold peg and the fixed-rate regime. Issues reserve currency and collects seigniorage on dollar holdings by foreign central banks. Defines the discount rate and can unilaterally alter the terms of convertibility. Denominated its foreign liabilities in its own currency, making exchange-rate adjustment painless relative to other states' exposure.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuer, beneficiary).

% Nations like Britain, France, and later developed Europe and Japan. Denominated foreign liabilities in foreign currency (dollars) while their own currencies bore the adjustment burden. Locked into fixed parities with the dollar, unable to devalue unilaterally without negotiating with the IMF and the U.S. Treasury. Subordinated domestic monetary policy to exchange-rate defense and gold-reserve management.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, generational, constrained, global).

% Capital-rich states and corporations operating across fixed-parity zones. Predictable exchange rates reduce hedging costs for cross-border investment and lending. Portfolio capital can move across borders without currency depreciation risk, capturing interest differentials with certainty.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, capital_exporting_states, beneficiary,
    powerful, biographical, arbitrage, global).

% Developing nations and commodity exporters pegged to the dollar or constrained by the IMF Articles. Prices for their exports (agricultural, raw materials) denominated in dollars; prices of imports (manufactured, capital equipment) also in dollars. Unable to devalue to improve competitiveness without IMF approval and foreign-exchange reserve drain. Subject to discipline via IMF conditionality.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, peripheral_economies, payer,
    powerless, generational, trapped, global).

% Nations that rely on low-wage labor export and remittances (post-colonial economies, labor suppliers). Exchange controls and capital account restrictions under Bretton Woods limit their ability to price their labor competitively or retain earnings. Pegged currencies can overvalue domestically produced goods relative to labor, creating unemployment.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, labor_importing_states, payer,
    moderate, biographical, constrained, national).

% Administers the fixed-rate regime, approves devaluations, conditions lending on domestic austerity and trade liberalization. Enforces discipline on non-reserve-currency states through conditionality. Boards dominated by U.S. voting power (16.5% at founding, veto on major decisions) and European states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_governance_board, agenda_setter,
    institutional, generational, analytical, global).

% Central banks holding dollar reserves and gold. Exchange-rate stability depends on confidence in U.S. gold convertibility. Structural conflict: dollar-denominated reserves earn interest, but gold does not; conversion creates runs. Central banks are locked into holding dollars or accepting currency risk.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gold_reserve_holders, observer,
    institutional, generational, constrained, global).

% Worker organizations in non-reserve-currency states. Excluded from negotiations over fixed parities and exchange-rate defense. Exchange-rate overvaluation can price their labor out of global markets; IMF conditionality often requires wage restraint and labor-market deregulation as adjustment mechanisms.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, domestic_labor_movements, excluded,
    organized, biographical, constrained, national).

% Economists and policymakers in non-reserve-currency states advocating demand-side stimulus. Excluded from setting monetary policy because external balance (reserve preservation) dominates internal balance (full employment). Exchange-rate constraint subordinates fiscal policy to external discipline.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_demand_advocates, excluded,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-WWII problem of international monetary chaos: provides a common denominator (gold via dollar peg) for cross-border settlement, eliminates tariff-war competitive devaluations that devastated the 1930s, and creates a framework for capital mobility without constant currency realignment.
% TRANSFER_FUNCTION: Moves seigniorage and policy autonomy from non-reserve-currency states to the U.S. Reserve currency issuance extracts real resources (foreign economies hold and earn nothing on dollar reserves; the U.S. earns seigniorage on every dollar in foreign vaults). Exchange-rate adjustment burden falls asymmetrically on states that must devalue in foreign currency, not on the U.S., which can absorb reserve-currency overvaluation.
% ABSENT_VOICES: Labor movements in peripheral and non-reserve-currency states. Keynesians advocating full-employment policy. Developing-nation finance ministers who lose control of exchange rates. Rival reserve-currency candidates (sterling, franc) that are subordinated to dollar hegemony under the regime. These voices are structurally excluded from the Bretton Woods negotiations and the IMF board voting weights favoring U.S. and Western Europe.
% DISAPPEARANCE_RATIONALE: If Bretton Woods vanished overnight, flexible exchange rates would return, monetary autonomy would flow back to non-reserve-currency states, seigniorage would redistribute across currencies, and the U.S. would lose its ability to finance deficits through reserve accumulation. The entire post-war architecture of U.S. financial dominance rests on this regime.
% FOUNDING_PROBLEM: Post-WWII international monetary disorder: no common medium of exchange for settlement, competitive devaluations and tariff wars of the 1930s had destroyed trade, and new reconstruction required capital flows that currency chaos made impossible. The gold standard had collapsed; a return to pure gold was deflationary and impossible given the asymmetry of U.S. gold holdings. A new framework was needed to provide confidence in exchange-rate stability and international lending.
% FOUNDING_PROBLEM_CORROBORATION: The IMF and U.S. Treasury attested the founding problem was urgent and Bretton Woods solved it (growth, trade expansion, stability during reconstruction). By the 1960s, non-reserve-currency states, Keynesians (e.g., Joan Robinson, Triffin), and labor economists attested the founding problem was substantially SOLVED but the regime persisted as an asymmetric extraction mechanism: currencies had stabilized, capital flowed, trade expanded—yet the constraint's enforcement machinery (IMF conditionality, exchange-rate defense requirements) intensified precisely when the coordination justification weakened. Systematic scholarly critique (Triffin Dilemma, 1960) from outside the benefiting parties established the problem was displaced, not solved.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from 0.35 (1944, founding coordination focus) to 0.68 (1971, extraction dominance). This trajectory reflects how the regime shifted from solving genuine post-war monetary chaos to defending U.S. financial hegemony. By the 1960s, the Triffin Dilemma (U.S. unable to simultaneously maintain gold convertibility and dollar dominance) became public knowledge, yet the regime persisted through IMF enforcement and U.S. pressure on allies to hold dollars. Theater_ratio rises from 0.20 to 0.42, indicating growing performative activity: the gold anchor remained symbolically central to legitimacy, but enforcement increasingly relied on threats (IMF conditionality, capital controls policing, implicit U.S. veto over devaluations). Suppression_requirement plateaus at 0.71 after 1960, suggesting the enforcement intensity reached a ceiling—the IMF conditionality, capital controls, and exchange-rate defense requirements were at maximum sustainable levels; increasing them further would have triggered breakdowns (which did occur: sterling crisis 1967, French gold withdrawals 1965, U.S. gold run 1968). Non-reserve-currency states bore the suppression: constrained monetary policy, mandatory austerity programs, persistent unemployment from overvalued currencies. The U.S. faced only the threat of gold runs (suppression on the U.S. side was low, ~0.30), which it managed through the Swap Lines, Gold Pool, and ultimately unilateral default (1971).
 *
 * PERSPECTIVAL GAP:
 *   The reserve_currency_issuer seat experiences this as genuine coordination it maintains for global welfare—the frame held by the U.S. Treasury and IMF leadership (early speeches emphasizing reconstruction, stability, avoiding 1930s chaos). The non_reserve_currency_states seat experiences mounting asymmetry: early gratitude for stability (1944–1950) turned to resentment by the 1960s as the regime's extraction machinery became visible (Triffin analysis, sterling devaluation forced negotiations, French withdrawal of dollars for gold). The peripheral_economies seat experiences pure constraint: no autonomy, IMF-mandated deflation regardless of domestic unemployment, commodity prices denominated in dollars while import prices rose. The labor_importing_states seat experiences chronic overvaluation and excluded policy voice. The engine should compute different types at each seat: the U.S. seat computes this as coordination (low d, beneficiary position); the non-reserve seat computes tangled_rope or snare (high d, target position); the peripheral seat computes snare (very high d, no coordination benefit, pure extraction and suppression). The authored claim (tangled_rope) reflects the aggregate structure; the engine's per-seat computation reveals the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is highly heterogeneous across stakeholder seats. The reserve_currency_issuer has d ≈ 0.15 (full beneficiary: seigniorage, policy autonomy, ability to run deficits financed by foreign dollar accumulation). Non_reserve_currency_states have d ≈ 0.72 (mostly targets: subordinated exchange-rate defense, loss of monetary autonomy, but some coordination benefit from trade expansion). Peripheral_economies have d ≈ 0.88 (nearly full targets: no monetary policy autonomy, IMF conditionality, commodity-price subordination, trapped exit). Capital_exporting_states have d ≈ 0.20 (beneficiaries: predictable exchange rates reduce hedging costs, portfolio capital can move across borders without depreciation risk). Labor movements have d approaching 1.0 (targets: excluded from policy, bear costs of overvaluation via unemployment, no exit). This heterogeneity is NOT resolved by per-stakeholder directionality overrides because the engine derivation from beneficiary/victim declarations and exit_options should capture most of it. Override would be needed only if a stakeholder's structural relationship diverged from its declared role—e.g., if a stakeholder nominally a payer actually captured significant gains (regulatory capture). In this case, the declarations align with structural positions, so derivation should suffice. Commentary documents the heterogeneity to explain why the engine will compute dramatically different type verdicts at different seats.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the mandate outliving its function) is the diagnostic linchpin here. The founding problem was post-WWII monetary chaos; by 1960, that problem was substantially solved—growth was rapid, trade was expanding, capital flowed, exchange rates had stabilized. Yet the constraint persisted and intensified. The Triffin Dilemma analysis (published 1960, external to U.S. Treasury framing) showed that the founding mandate (stable exchange rates + capital flows) was becoming logically incompatible with the mechanism (fixed dollars + gold convertibility). Despite public acknowledgment of the incompatibility, the regime persisted through IMF enforcement and U.S. pressure. This is classic mandatrophy: the founding problem (1930s chaos) was replaced by a new problem (U.S. financial hegemony maintenance), but the mandate-and-mechanism stayed labeled with the old problem. The regime collapsed not from loss of coordination function but from loss of credibility in the mechanism—the U.S. suspended gold convertibility (1971) because the founding mechanism could no longer sustain both the mandate (global cooperation) and the extractive operation (U.S. seigniorage and policy autonomy). Mandatrophy is NOT resolved by reclassification; it is resolved by explicit acknowledgment that the constraint serves a new function (extraction/hegemony maintenance) that the original mandate does not justify. The constraint should be authored as 'mandatrophy resolved: false' or a statement that the regime persisted despite mandate displacement—the engine's mandatrophy_resolved flag and the computed type divergence (claim vs. actual) will signal this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gold_anchor_natural_or_constructed,
    'Is the gold peg a genuine natural anchor (discipline imposed by scarcity) or a constructed constraint maintained by U.S. control and central bank coordination?',
    'Examine the mechanics of gold-peg maintenance: were gold prices set by market or decree? Did the U.S. manipulate gold inflows/outflows to defend the peg? Was the Gold Pool a ''natural'' market-clearing mechanism or a price-fixing cartel?',
    'If natural (scarcity discipline), the regime is a mountain with distributed targets; if constructed (U.S.-maintained), it is a snare with U.S. as beneficiary and others as victims. The ε referent shifts from the equilibrium arrangement to the asymmetric enforcement arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gold_anchor_natural_or_constructed, empirical, 'Whether the gold anchor represents natural monetary discipline or constructed U.S. hegemony.').

omega_variable(
    coordination_extraction_decomposition,
    'Can the genuine coordination function (post-1930s chaos avoidance, trade stability) be separated from the extraction function (seigniorage, policy subordination), or are they structurally intertwined?',
    'Counterfactual institutional design: could an alternative regime have provided the coordination benefits without the extraction? (E.g., a symmetrical reserve-currency system with no single issuer, or a managed float with IMF rules.) Empirical test: did non-reserve-currency states benefit more from coordination than they lost to extraction? (Compare growth trajectories with estimates of seigniorage transfer and policy-constraint costs.)',
    'If separable, Bretton Woods is a snare (extraction riding on coordination cover); if inseparable, it is a tangled_rope (genuine hybrid). If extraction exceeded coordination benefit for most of the period, the constraint should be reclassified as snare from 1960 onward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_decomposition, conceptual, 'Whether Bretton Woods coordination and extraction functions are structurally separable.').

omega_variable(
    mandate_displacement_over_interval,
    'Did the regime''s founding mandate (avoiding 1930s-style chaos) remain operative over the 1944–1971 interval, or was it displaced by a secondary mandate (U.S. hegemony maintenance) after 1960?',
    'Examine policy discourse over time: did U.S. Treasury and IMF leadership justify the regime based on post-war chaos prevention (1944–1955) or on global monetary stability (1955–1960) or on U.S. financial leadership (1960–1971)? When did framing shift from ''collective security'' to ''defending the dollar''?',
    'If mandate was displaced but constraint persisted, the regime exhibits mandatrophy and should be marked ''mandatrophy_resolved: false''. This would reclassify the later period (1960–1971) as snare with U.S. hegemony extraction as the primary function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_displacement_over_interval, empirical, 'Whether the constraint''s operative mandate shifted over the interval, indicating mandatrophy.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of monetary autonomy in non-reserve-currency states structural (external IMF enforcement, capital controls, exchange-rate mechanics) or internalized (elites'' acceptance of the regime as legitimate and unchallengeable)?',
    'Post-regime analysis: did states that exited Bretton Woods (or were exempted, like post-colonial nations) quickly recover monetary autonomy and different policy outcomes? Did labor movements and opposition parties immediately call for exit once the regime''s constraints became visible (1960s), or did acceptance persist?',
    'If structural, exit is possible and the constraint is sustained only by external enforcement (snare); if internalized, elites carry the suppression even after exit, requiring deprogramming (constraint is partially internalized and harder to escape). This affects both the classification and the remediation strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of monetary autonomy is structural or internalized in non-reserve-currency states.').

omega_variable(
    exorbitant_privilege_as_seigniorage_or_policy_option,
    'Does U.S. ''exorbitant privilege'' manifest primarily as seigniorage (direct revenue from foreign dollar holdings) or as policy autonomy (ability to run deficits and adjust unilaterally)?',
    'Quantify seigniorage flows: calculate foreign central-bank dollar holdings and the implicit transfer to the U.S. per year. Compare to U.S. policy autonomy: did the U.S. devalue relative to gold or need IMF approval for exchange-rate changes? (Answer: no, the U.S. set the gold price unilaterally; other states had to negotiate devaluation through IMF.)',
    'If seigniorage dominates, the U.S. is a pure rent-collector beneficiary (snare framing). If policy autonomy dominates, the U.S. benefits from reduced external discipline (rope framing for U.S., snare for others). Most evidence suggests BOTH are significant; the referent for ε should encompass both flows.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exorbitant_privilege_as_seigniorage_or_policy_option, empirical, 'Whether U.S. privilege under Bretton Woods manifests as seigniorage revenue or policy autonomy (or both).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_sovdef_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.2).
narrative_ontology:measurement_basis(bw_sovdef_tr_t1944, projected).
narrative_ontology:measurement(bw_sovdef_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.28).
narrative_ontology:measurement_basis(bw_sovdef_tr_t1950, observed).
narrative_ontology:measurement(bw_sovdef_tr_t1955, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1955, 0.32).
narrative_ontology:measurement_basis(bw_sovdef_tr_t1955, observed).
narrative_ontology:measurement(bw_sovdef_tr_t1960, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1960, 0.38).
narrative_ontology:measurement_basis(bw_sovdef_tr_t1960, observed).
narrative_ontology:measurement(bw_sovdef_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.41).
narrative_ontology:measurement_basis(bw_sovdef_tr_t1965, observed).
narrative_ontology:measurement(bw_sovdef_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.42).
narrative_ontology:measurement_basis(bw_sovdef_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(bw_sovdef_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.35).
narrative_ontology:measurement_basis(bw_sovdef_be_t1944, projected).
narrative_ontology:measurement(bw_sovdef_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement_basis(bw_sovdef_be_t1950, observed).
narrative_ontology:measurement(bw_sovdef_be_t1955, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1955, 0.55).
narrative_ontology:measurement_basis(bw_sovdef_be_t1955, observed).
narrative_ontology:measurement(bw_sovdef_be_t1960, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1960, 0.62).
narrative_ontology:measurement_basis(bw_sovdef_be_t1960, observed).
narrative_ontology:measurement(bw_sovdef_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.67).
narrative_ontology:measurement_basis(bw_sovdef_be_t1965, observed).
narrative_ontology:measurement(bw_sovdef_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.68).
narrative_ontology:measurement_basis(bw_sovdef_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(bw_sovdef_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement_basis(bw_sovdef_su_t1944, projected).
narrative_ontology:measurement(bw_sovdef_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.64).
narrative_ontology:measurement_basis(bw_sovdef_su_t1950, observed).
narrative_ontology:measurement(bw_sovdef_su_t1955, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1955, 0.68).
narrative_ontology:measurement_basis(bw_sovdef_su_t1955, observed).
narrative_ontology:measurement(bw_sovdef_su_t1960, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1960, 0.71).
narrative_ontology:measurement_basis(bw_sovdef_su_t1960, observed).
narrative_ontology:measurement(bw_sovdef_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.72).
narrative_ontology:measurement_basis(bw_sovdef_su_t1965, observed).
narrative_ontology:measurement(bw_sovdef_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.71).
narrative_ontology:measurement_basis(bw_sovdef_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, global_infrastructure).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__sovereignty_defense, 0.25).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_structural_adjustment).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, reserve_currency_hegemony_mechanism).

% DUAL FORMULATION NOTE:
% The Bretton Woods treaty substrate decomposes into three structurally distinct constraints corresponding to three readings: sovereignty_defense (this story), keynesian_embedded_liberalism, and neoliberal_convertibility. Each reading has a different ε, different beneficiary/victim structure, and different classification. The three are linked by the kernel (the fixed-exchange-rate gold-backed regime) but represent different interpretations of its mandate and mechanism. Sovereignty_defense emphasizes national monetary independence and the gold anchor's role in defending it; keynesian_embedded_liberalism emphasizes capital controls and policy space; neoliberal_convertibility emphasizes discipline on government and freedom for capital. The three readings coexist as live positions in policy discourse (they are not logically foreclosed). This story (sovereignty_defense) influences the neoliberal_convertibility reading by creating pressure to remove capital controls and tighten the discipline mechanism; it coexists with the keynesian_embedded_liberalism reading as different institutional actors' framings of the same arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
