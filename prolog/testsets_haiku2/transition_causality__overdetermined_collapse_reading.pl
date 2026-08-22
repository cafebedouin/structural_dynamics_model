% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__overdetermined_collapse_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Fixed-Rate Regime Under Overdetermined Structural Collapse
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint instantiates the overdetermined_collapse_reading of the
 *   transition_causality kernel: the Bretton Woods fixed-rate regime under a
 *   US-centered dollar standard contained multiple reinforcing structural
 *   contradictions (Triffin Dilemma, gold-loss constraint, policy trilemma,
 *   capital-export demands) such that collapse was logically inevitable
 *   regardless of policy choices, trigger events, or leadership decisions.
 *   Under this reading, the 1971 transition to flexible rates was not a
 *   policy option that could have been avoided; it was a forced resolution of
 *   contradictions that had become mathematically unsustainable by the
 *   mid-1960s. The constraint is the fixed-rate regime itself, assessed as an
 *   extraction arrangement that extracted from deficit governments and gold
 *   holders while benefiting the US and capital exporters, and persisted only
 *   through progressively more desperate enforcement (gold drain, capital
 *   controls, IMF discipline) until it became structurally impossible to
 *   maintain. The reading does NOT claim policy choices were irrelevant; it
 *   claims they were constrained by the structure such that the outcome was
 *   determined once the regime was constructed, regardless of which policies
 *   were chosen within the available set.
 *
 * KEY AGENTS:
 *   - US Treasury and Federal Reserve: agenda-setter, institutional power, high arbitrage exit, sets and enforces the gold peg and reserve-currency rules
 *   - Deficit-constrained governments (France, UK, Germany, Canada): payers, powerful at home but trapped by peg, face zero policy autonomy
 *   - Fixed-rate reserve holders (central banks globally): organized, constrained exit, accumulate dollars but cannot redeem without triggering crisis
 *   - Gold-losing sovereigns (especially France under de Gaulle): payers, attempt unilateral conversion after 1965, expose the drain mechanism
 *   - Capital exporters (US multinationals, banks, portfolio investors): beneficiaries, powerful, mobile, arbitrage the stable peg and capital openness
 *   - Emerging markets and periphery: powerless, trapped by fixed rates imposed by core, compressed by structural adjustment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.92).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.78).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Fixed-Rate Regime Under Overdetermined Structural Collapse").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '5862a03c-10ef-4d41-8e9e-d279105d5715').
narrative_ontology:cs_kernel_codification('5862a03c-10ef-4d41-8e9e-d279105d5715', fixed_text).
narrative_ontology:cs_authority_grounding('5862a03c-10ef-4d41-8e9e-d279105d5715', extraction).
narrative_ontology:cs_interpretation_layer_present('5862a03c-10ef-4d41-8e9e-d279105d5715').
narrative_ontology:cs_reading_relation('5862a03c-10ef-4d41-8e9e-d279105d5715', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('5862a03c-10ef-4d41-8e9e-d279105d5715', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('5862a03c-10ef-4d41-8e9e-d279105d5715', foundational, structural_overdetermination_closure).
narrative_ontology:cs_axiom_status(structural_overdetermination_closure, holdable).
narrative_ontology:cs_axiom_grounding('5862a03c-10ef-4d41-8e9e-d279105d5715', structural_overdetermination_closure, empirically_contingent).
narrative_ontology:cs_axiom('5862a03c-10ef-4d41-8e9e-d279105d5715', foundational, counterfactual_viability_near_zero).
narrative_ontology:cs_axiom_status(counterfactual_viability_near_zero, holdable).
narrative_ontology:cs_axiom_grounding('5862a03c-10ef-4d41-8e9e-d279105d5715', counterfactual_viability_near_zero, empirically_contingent).
narrative_ontology:cs_reference_frame('5862a03c-10ef-4d41-8e9e-d279105d5715', bretton_woods_articles_of_agreement).
narrative_ontology:cs_drift_state('5862a03c-10ef-4d41-8e9e-d279105d5715', mid_1960s_accumulated_contradictions, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5862a03c-10ef-4d41-8e9e-d279105d5715', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, dollar_reserve_currency_beneficiaries).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, capital_exporters_us_aligned).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, deficit_constrained_governments).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_rate_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_losing_sovereigns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, fixed_rate_reserve_holders).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, international_capital_exporters).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, emerging_markets_and_periphery).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the gold peg and defends it through open-market operations, manages the dollar as reserve currency, and controls the IMF governance structure. Bears the cost of maintaining the peg as gold flows out and as the US fiscal position deteriorates (Vietnam War spending causes inflation while the peg constrains monetary accommodation). Has the option to abandon the peg (as Nixon does in 1971) and does not face capital flight because countries cannot exit without triggering the regime's collapse.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_treasury_and_fed, agenda_setter,
    institutional, generational, arbitrage, global).

% France, UK, Germany, Canada and others run persistent current-account deficits and must maintain fixed pegs to the dollar. Cannot devalue unilaterally (IMF rules forbid it except in 'fundamental disequilibrium,' a rare exception). Cannot use monetary policy to respond to domestic inflation or unemployment because the peg constrains the money supply. Must build dollar reserves to defend the peg, which means exporting real resources to the US. If they unilaterally convert dollars to gold or abandon the peg, they trigger the system-wide collapse and invite capital flight and currency crisis.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, deficit_constrained_governments, payer,
    powerful, biographical, trapped, national).

% Central banks of Belgium, Netherlands, Switzerland, West Germany accumulate dollars as reserves and hold them in US Treasury securities. Receive interest payments and the benefit of stable, predictable international pricing in dollars (beneficiary side). Bear inflation losses as the purchasing power of their reserves erodes due to rising US inflation in the 1960s. Cannot withdraw reserves and convert to gold en masse (triggers the run) but watch the gold drain accelerate as other central banks attempt unilateral conversions.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_rate_reserve_holders, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, fixed_rate_reserve_holders, beneficiary).

% Hold gold reserves that progressively drain to the US Treasury as foreign governments exercise the right to convert dollars to gold under the Bretton Woods guarantee. France begins aggressive conversion after 1965 (de Gaulle calls for a return to gold standard and converts dollars back). They lose gold (a real asset) while the US loses only paper dollars (no real cost to the payer). Lack individual exit: if they stop the conversion pipeline, they appear weak; if they convert aggressively, they accelerate the crisis.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_losing_sovereigns, payer,
    powerful, generational, constrained, national).

% US multinational corporations, banks, and portfolio investors deploy capital globally without currency risk under the fixed-rate regime. They can invest in foreign securities, repatriate earnings to dollars, and arbitrage price differences across currencies without hedging costs. The regime subsidizes their capital export and ensures stable returns in dollars.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_capital_exporters, beneficiary,
    powerful, biographical, arbitrage, global).

% Accept fixed exchange rates imposed by core countries without negotiating power. Cannot use devaluation to manage inflation or export-led growth. Must accept IMF structural adjustment that enforces fiscal and monetary discipline, often at the cost of domestic economic policy autonomy. Have zero voice in IMF governance or the gold-peg decision. Accumulate imports of capital goods under terms set by core countries.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, emerging_markets_and_periphery, payer,
    powerless, biographical, trapped, global).

% The IMF, World Bank, and the Articles of Agreement enforce fixed rates, capital controls, and US-centered governance. The institutional apparatus persists because it has no option to reform itself (reform requires majority vote controlled by the US) and its mandate (prevent competitive devaluations) is still technically valid even though the founding problem is solved. The machinery becomes self-perpetuating: the IMF survives by enforcing discipline; discipline becomes the constraint's primary function.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, bretton_woods_institutional_machinery, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(transition_causality__overdetermined_collapse_reading, bretton_woods_institutional_machinery).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, us_treasury_and_fed).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilize international monetary relations and capital flows after WWII fragmentation: fixed exchange rates reduce transaction costs and currency risk for multilateral trade; dollar as reserve currency simplifies three-way settlement (A trades with B in dollars, B with C in dollars, C with A in dollars) without requiring complex bilateral rates; gold convertibility anchors confidence in the dollar's value; IMF surveillance prevents competitive devaluations and capital flight.
% TRANSFER_FUNCTION: Transfers seigniorage from the rest of the world to the US Treasury (value of newly printed dollars accepted as reserves); extracts real resources from deficit governments forced to compress domestic spending to maintain the peg; draws gold from foreign central banks to the US Treasury as they exercise conversion rights; channels investment returns from high-growth deficit countries back to US capital exporters; constrains policy autonomy for everyone except the US.
% ABSENT_VOICES: Domestic constituencies in deficit countries that bear the compression costs (labor movements facing unemployment and wage pressure from tight monetary policy; public-spending constituencies denied fiscal expansion; voters who cannot devalue to improve employment). Emerging markets' development ministries, which would argue for policy autonomy but have no seat in IMF governance. International economists who see the logical contradictions (Triffin publishes in 1960; Kindleberger documents instability; but their analyses are sidelined as long as the peg holds).
% DISAPPEARANCE_RATIONALE: If the Bretton Woods constraint (dollar peg, fixed rates, capital-account openness under dollar standard) had never been imposed or had been abandoned earlier (say, 1960), the monetary order would have reorganized into a flexible-rate system or a multi-currency reserve standard much sooner. The 1971 transition proves the world does reorganize — floating rates, multiple reserve currencies, capital controls reappear, policy autonomy returns to central banks. The constraint's removal is not neutral; it changes the entire structure of incentives and possibilities for every actor.
% FOUNDING_PROBLEM: Prevent the competitive devaluations, currency fragmentation, and destabilizing speculation of the 1930s and the breakdown of the gold standard during WWII. Create a stable, rules-based, symmetric framework for international trade and capital flows so post-war reconstruction could proceed without the uncertainty and strategic currency behavior that characterized the Depression and wartime.
% FOUNDING_PROBLEM_CORROBORATION: By 1955-1960, the founding problem is empirically solved: no competitive devaluations occur, the peg holds, international trade grows steadily. By 1960, Triffin publishes his work documenting that the system's contradictions are acute but the founding problem is no longer the constraint's raison d'être — the constraint now persists because institutional interests prefer it, not because the problem it was designed for is still live. IMF staff, US Treasury economists (including Roosa), and European central bankers by the mid-1960s all acknowledge that the gold peg is unsustainable without major reform; yet reform proposals (the Roosa Bonds, the Special Drawing Rights, proposals for a higher gold price) all fail because the US has no interest in sharing seigniorage and deficit governments lack leverage to force change. The constraint persists not because the founding problem is live but because the institutional beneficiaries have political power to maintain it despite its contradictions.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.92, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.58 to 0.92 over the interval (1945-1971, scaled to 0-26 years). This is not random drift; it reflects the measured compression of policy space as contradictions accumulate. Early in the regime (t=0), the extraction is partly hidden by genuine coordination benefits (international stability post-war) and by the fact that the peg is working (gold flows are manageable). By t=10 (circa 1955), the gold drain accelerates and France begins calling for gold-based reserves instead of dollars — extraction is visible but governments still have incomplete information about the trajectory. By t=16 (circa 1961), the London Gold Pool is formed to suppress gold-market prices, a pure theatrical intervention; extraction is now obvious but enforcement machinery intensifies. By t=20 (circa 1965), the Vietnam War fiscal pressures spike US inflation while the peg holds — the Triffin contradiction is acute and policymakers know it. By t=26 (1971), the peg collapses not because policies failed but because the underlying math became impossible. Suppression requirement rises from 0.45 to 0.78 over the same interval, tracking the progressive enforcement intensity: capital controls tighten, gold market interventions expand, IMF discipline hardens. Theater ratio is low throughout (peaks at 0.22) because the enforcement is real — gold actually leaves the Treasury, actual capital controls are imposed, the IMF actually enforces discipline — but it grows because more enforcement activity is devoted to suppressing the visible contradictions rather than solving the founding problem. The measurement grid is shared across all three metrics; every time point carries all three values.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury seat: Bretton Woods is a coordination mechanism that the US designed, pays to maintain, and in which the US has taken on the burden of reserve-currency supply and gold-convertibility guarantees. The extraction the other reading perceives is, from this seat, the price those countries pay for access to a stable system; the US is subsidizing stability. This reading is not irrational — the US does bear real costs (gold outflows, fiscal constraint, eventually political isolation when it must abandon the peg). From the deficit-government seat: Bretton Woods is a mechanism by which the US imposes policy constraints on everyone else, extracts seigniorage through dollar inflation, and forces other countries to hold dollar reserves they cannot redeem. The US designed it, the US benefits, and the US uses it to manage the global system to US advantage. This reading is also not irrational — the historical record shows the US doing exactly that. Under the overdetermined reading, BOTH are correct AND both are trapped: the US really is bearing costs AND really is extracting benefits, and the deficit governments really are constrained AND really are subsidizing the US. The structure is not asymmetric because one side is wrong; it is asymmetric because the constraints are lopsided — the US has arbitrage exit (it can always abandon the peg, as it does in 1971) while deficit countries do not (they collapse if they unilaterally exit). This is what makes it extractive despite genuine coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Under the overdetermined reading, directionality flows from structural position, not from the policy choices of actors. The US Treasury/Fed sits at d≈0.0 (full beneficiary): the arrangement subsidizes US fiscal expansion, lets the US export capital freely, and lets the US extract seigniorage from the rest of the world's reserve accumulation. Deficit governments sit at d≈1.0 (full target): they are trapped by the peg, cannot use currency policy to respond to shocks, and must compress domestic spending to maintain reserves. Gold losers sit at d≈0.95 (near-full target): they lose gold reserves progressively with zero autonomy to stop the drain. Reserve holders sit at d≈0.6 (asymmetric): they get interest on dollar reserves (beneficiary side) but lose purchasing power from dollar inflation and cannot exit without triggering the crisis (target side). Capital exporters sit at d≈0.1 (near-beneficiary): they move freely while others are constrained. The directionality is high (χ high for targets, negative for beneficiaries) because the structure is not consensual — actors are trapped not by lack of exit (some, like France, have high-cost-but-possible exit) but by the fact that unilateral exit causes the system to collapse, imposing costs on everyone. This is the structure of a tangled rope with strong asymmetry: coordination function (stable exchange rates) yoked to extraction mechanism (seigniorage, policy constraint, gold drain) and held together by active enforcement (capital controls, IMF surveillance, gold-market intervention). The beneficiary seats compute the constraint as a rope (genuine coordination it benefits from); the payer seats compute it as a snare (extraction masked by coordination rhetoric). The engine computes per-seat; this commentary explains why the divergence is structural and not perceptual.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was preventing 1930s-style competitive devaluations and currency fragmentation. By 1960, this problem was completely solved — the fixed rates held rock-solid, there were no competitive devaluations, international trade grew steadily. The regime's mandate was accomplished. Yet it persisted for another decade, not because the founding problem remained live but because the institutional machinery became self-perpetuating: the IMF had an enforcement role, central banks had dollar reserves as their primary asset, the US had a structural interest in maintaining seigniorage, and exit was collectively irrational (unilateral exit by any country except the US triggered crisis). This is mandatrophy: a coordination mechanism whose original function has been solved but which persists as an extraction machine because the institutional interests aligned to maintain it are stronger than the pressure to change it. The founding_problem_status of 'dead' and the disappearance_verdict of 'world_rearranges' together diagnose mandatrophy: if the regime disappeared, the world would reorganize NOT because the founding problem re-emerged, but because the regime's persistence was now a problem, not a solution. A flexible-rate system would emerge not to prevent competitive devaluations (already prevented) but to allow policy autonomy for trapped governments. This reading of the transition frames it not as a failure of the original system but as the inevitable endpoint of a system that had succeeded at its task and then been captured by extractive institutional interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermined_vs_contingent_boundary,
    'Were the structural contradictions (Triffin Dilemma, gold drain, policy constraint trilemma) truly overdetermined — i.e., any one of them alone would have forced transition — or did they require contingent trigger events (Vietnam War fiscal pressures, speculative runs on gold) to actualize collapse?',
    'Counterfactual analysis: model the regime without the Vietnam War fiscal stimulus, without speculative confidence crashes, without de Gaulle''s 1965 conversion. If the system would still have collapsed from pure contradiction alone, overdetermination holds; if at least one trigger was necessary, the hybrid reading is correct.',
    'If overdetermined, this reading holds as structurally necessary. If contingent triggers were required, the constraint should be reclassified to hybrid_trigger_reading (tangled rope with path-dependent collapse). The two readings are logically incompatible within the same causal frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermined_vs_contingent_boundary, conceptual, 'Whether collapse was logically determined by structural contradictions alone or required contingent historical events.').

omega_variable(
    counterfactual_viability_of_reform,
    'Could Bretton Woods have been reformed in place (e.g., via a new SDR-based reserve standard, or a higher gold price in the 1960s) instead of collapsing, and if so, why did policymakers choose not to pursue reform?',
    'Historical archival analysis of IMF discussions, Treasury memos, and central bank correspondence 1960-1970. Document whether reform proposals existed, what their structural obstacles were, and why political economy blocked them despite technical feasibility.',
    'If reform was structurally feasible but politically blocked, the reading shifts toward snare (extraction maintained by coercive gatekeeping despite functional alternatives). If reform was logically foreclosed by the Triffin contradiction itself, overdetermination holds. This addresses whether the constraint''s persistence was inevitable or chosen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_reform, empirical, 'Whether structural reform was technically viable but politically blocked, or structurally impossible.').

omega_variable(
    reading_kernel_decomposition,
    'This constraint is one reading of the transition_causality kernel. How do the overdetermined_collapse, contingent_choice, and hybrid_trigger readings relate structurally? Are they genuinely foreclosed against each other, or do they coexist as competing interpretations of the same event?',
    'Analyze the logical incompatibilities: overdetermined (causality determined by structure alone) forecloses contingent_choice (policy could have prevented it) only if one accepts that structural necessity means no choice mattered. But a weaker reading of contingent_choice (the choice existed even if the outcome was overdetermined) might coexist. Hybrid_trigger (both structure and contingency) occupies the middle ground and likely coexists with the other two if ''contingency'' means necessary conditions rather than sufficient ones.',
    'If readings are genuinely foreclosed, only one can be correct; the engine routes to the committer frame''s axioms and reference_frame to compute which. If they coexist, the disagreement is about emphasis (which causal factors matter most) rather than logical incompatibility. This affects how the corpus treats the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_decomposition, conceptual, 'Kernel reading relationship: foreclosure vs. coexistence vs. influence.').

omega_variable(
    victim_structure_under_inevitability,
    'If the transition was overdetermined and inevitable, are all actors constrained by the fixed-rate regime structurally victimized (bearing unavoidable costs they did not author), or do some of them (e.g., deficit governments that chose to join Bretton Woods) bear costs they could have avoided by not joining?',
    'Examine the historical record of accession: were deficit-constrained governments coerced into joining (neocolonial dependency), or did they choose membership for its benefits and accept the constraints knowingly? If coerced, they are true victims of an overdetermined structure. If voluntary but trapped, they are victims of their own past choice under imperfect information.',
    'True victimhood (coerced membership) supports this reading''s claim that the constraint extracts from structurally subordinate actors. Voluntary membership with posterior trap supports a snare reading (extraction maintained by gating exits after the choice). The distinction affects remedial logic: inevitability implies no party could have chosen differently; victim-choice implies some parties could have but didn''t.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_under_inevitability, empirical, 'Whether deficit-government membership in Bretton Woods was coerced or voluntary with posterior trapping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tran_tr_t2, transition_causality__overdetermined_collapse_reading, theater_ratio, 2, 0.09).
narrative_ontology:measurement(tran_tr_t4, transition_causality__overdetermined_collapse_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(tran_tr_t7, transition_causality__overdetermined_collapse_reading, theater_ratio, 7, 0.14).
narrative_ontology:measurement(tran_tr_t10, transition_causality__overdetermined_collapse_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(tran_tr_t13, transition_causality__overdetermined_collapse_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement(tran_tr_t16, transition_causality__overdetermined_collapse_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(tran_tr_t20, transition_causality__overdetermined_collapse_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(tran_tr_t26, transition_causality__overdetermined_collapse_reading, theater_ratio, 26, 0.22).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(tran_be_t2, transition_causality__overdetermined_collapse_reading, base_extractiveness, 2, 0.62).
narrative_ontology:measurement(tran_be_t4, transition_causality__overdetermined_collapse_reading, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(tran_be_t7, transition_causality__overdetermined_collapse_reading, base_extractiveness, 7, 0.74).
narrative_ontology:measurement(tran_be_t10, transition_causality__overdetermined_collapse_reading, base_extractiveness, 10, 0.82).
narrative_ontology:measurement(tran_be_t13, transition_causality__overdetermined_collapse_reading, base_extractiveness, 13, 0.87).
narrative_ontology:measurement(tran_be_t16, transition_causality__overdetermined_collapse_reading, base_extractiveness, 16, 0.9).
narrative_ontology:measurement(tran_be_t20, transition_causality__overdetermined_collapse_reading, base_extractiveness, 20, 0.92).
narrative_ontology:measurement(tran_be_t26, transition_causality__overdetermined_collapse_reading, base_extractiveness, 26, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tran_su_t2, transition_causality__overdetermined_collapse_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(tran_su_t4, transition_causality__overdetermined_collapse_reading, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(tran_su_t7, transition_causality__overdetermined_collapse_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(tran_su_t10, transition_causality__overdetermined_collapse_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement(tran_su_t13, transition_causality__overdetermined_collapse_reading, suppression_requirement, 13, 0.75).
narrative_ontology:measurement(tran_su_t16, transition_causality__overdetermined_collapse_reading, suppression_requirement, 16, 0.77).
narrative_ontology:measurement(tran_su_t20, transition_causality__overdetermined_collapse_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(tran_su_t26, transition_causality__overdetermined_collapse_reading, suppression_requirement, 26, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.22).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_mountain).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, dollar_seigniorage_extraction).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, gold_standard_policy_trilemma).

% DUAL FORMULATION NOTE:
% This constraint story instantiates one reading of the transition_causality kernel. The sibling readings (contingent_choice and hybrid_trigger) are SEPARATE constraint stories with the same referent (the 1971 transition) but different ε values and causal structures. All three stories link via network.affects_constraints to enable comparative analysis: the engine can measure how different readings of the same kernel produce different per-seat classifications. The Triffin Dilemma is authored separately as a mountain constraint (the logical contradiction is structural); this story treats the Triffin constraint as one element in an overdetermined causal field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__overdetermined_collapse_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
