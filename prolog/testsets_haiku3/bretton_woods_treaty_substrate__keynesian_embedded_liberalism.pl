% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_keynesian_embedded_liberalism, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_treaty_substrate__keynesian_embedded_liberalism
 *   human_readable: Bretton Woods Capital Controls: Embedded Liberalism Reading
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   The Bretton Woods Agreement (1944) established a multilateral framework
 *   for international monetary relations centered on fixed exchange rates
 *   pegged to the US dollar and the dollar's fixed price in gold (35 USD/oz).
 *   A central feature—often obscured in modern discourse—was the explicit
 *   legitimation of capital controls on cross-border financial flows. This
 *   constraint story instantiates the Keynesian embedded-liberalism reading:
 *   Bretton Woods is a coordination mechanism that enables international
 *   trade while protecting national governments' policy autonomy by
 *   constraining international finance. In this reading, capital controls are
 *   not violations of free-market principles but legitimate tools that member
 *   states author to balance external monetary discipline against domestic
 *   welfare-state policy space. This reading is one of three contested
 *   interpretations of the same kernel: the neoliberal reading emphasizes
 *   that Bretton Woods constrains government intervention to enable capital
 *   markets; the sovereignty-defense reading emphasizes that it constrains
 *   external monetary discipline to preserve national sovereignty. The three
 *   readings are not compatible within a single framework—they make
 *   contradictory claims about WHO benefits and WHO pays—and they have
 *   different empirical footprints. This document generates the
 *   embedded-liberalism reading as a clean, ε-invariant constraint without
 *   hedging across readings or averaging ε values.
 *
 * KEY AGENTS:
 *   - National governments (organized beneficiaries): Author the constraint, retain policy space, use capital controls to protect domestic welfare systems.
 *   - International finance sector (powerful victims): Cannot freely arbitrage across borders; profit opportunities from financial intermediation are reduced; access to returns depends on IMF approval.
 *   - Labor unions and welfare constituencies (organized beneficiaries): Protected from capital flight when wages rise or governments expand welfare spending.
 *   - Multinational corporations (powerful victims): Cannot freely repatriate profits or relocate production to escape labor organization or taxation.
 *   - Speculative capital flows (trapped victims): Structurally blocked; the constraint is explicitly designed to prevent this flow type.
 *   - IMF governance structure (institutional agenda-setter): Administers the rules, interprets Article VI, grants exceptions, and conditions liquidity access on compliance.
 *   - United States (institutional beneficiary and agenda-setter): Dollar becomes reserve currency in a managed system; US gains both coordination benefits and asymmetric leverage.
 *   - Postcolonial nations (dual-positioned): Gain formal authority to impose capital controls but enter IMF governance with under-weighted voting power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.38).
domain_priors:suppression_score(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.22).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, extractiveness, 0.38).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "Bretton Woods Capital Controls: Embedded Liberalism Reading").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__keynesian_embedded_liberalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'c9bf2ede-23be-4eb1-b612-01b26edba641').
narrative_ontology:cs_kernel_codification('c9bf2ede-23be-4eb1-b612-01b26edba641', fixed_text).
narrative_ontology:cs_authority_grounding('c9bf2ede-23be-4eb1-b612-01b26edba641', lineage).
narrative_ontology:cs_interpretation_layer_present('c9bf2ede-23be-4eb1-b612-01b26edba641').
narrative_ontology:cs_reading_relation('c9bf2ede-23be-4eb1-b612-01b26edba641', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_reading_relation('c9bf2ede-23be-4eb1-b612-01b26edba641', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('c9bf2ede-23be-4eb1-b612-01b26edba641', foundational, capital_controls_legitimate_coordination_tool).
narrative_ontology:cs_axiom_status(capital_controls_legitimate_coordination_tool, holdable).
narrative_ontology:cs_axiom_grounding('c9bf2ede-23be-4eb1-b612-01b26edba641', capital_controls_legitimate_coordination_tool, deontological).
narrative_ontology:cs_axiom('c9bf2ede-23be-4eb1-b612-01b26edba641', foundational, domestic_policy_autonomy_prerequisite_welfare_state).
narrative_ontology:cs_axiom_status(domestic_policy_autonomy_prerequisite_welfare_state, holdable).
narrative_ontology:cs_axiom_grounding('c9bf2ede-23be-4eb1-b612-01b26edba641', domestic_policy_autonomy_prerequisite_welfare_state, empirically_contingent).
narrative_ontology:cs_axiom('c9bf2ede-23be-4eb1-b612-01b26edba641', secondary, international_finance_subordinate_to_governance).
narrative_ontology:cs_axiom_status(international_finance_subordinate_to_governance, holdable).
narrative_ontology:cs_axiom_grounding('c9bf2ede-23be-4eb1-b612-01b26edba641', international_finance_subordinate_to_governance, conventional).
narrative_ontology:cs_reference_frame('c9bf2ede-23be-4eb1-b612-01b26edba641', embedded_liberalism_constitutional_balance).
narrative_ontology:cs_drift_state('c9bf2ede-23be-4eb1-b612-01b26edba641', late_1960s_gold_reserve_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9bf2ede-23be-4eb1-b612-01b26edba641', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_unions).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_sector).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_capital_flows).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postcolonial_nations).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postcolonial_nations).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, embedded_liberalism_doctrine).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, capital_account_regulation_legitimacy).
narrative_ontology:constraint_vindicates(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_policy_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain discretion over monetary policy, capital allocation, and welfare-state funding by restricting cross-border capital movements. Can set domestic interest rates without immediate arbitrage pressure. Can direct credit to strategic sectors (industry, agriculture, housing) without foreign portfolio outflows. Collectively author the Bretton Woods rules through the IMF Articles of Agreement and manage exceptions through the Fund's approval process.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments, agenda_setter).

% Cannot freely arbitrage interest-rate differentials across national borders; must navigate capital account restrictions when seeking returns. Portfolio managers cannot route capital to highest-yield jurisdictions without regulatory permission and currency-conversion controls. The constraint channels investable capital through official IMF channels rather than free market discovery, reducing profit opportunities from financial intermediation and currency speculation.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, international_finance_sector, payer,
    powerful, biographical, constrained, global).

% Capital controls protect domestic wage levels by preventing capital flight when labor costs rise or strike activity occurs. Firms cannot easily move production offshore to escape labor organization. Governments retain policy space to negotiate wage bargains and expand welfare provisions without immediate capital flight penalty.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, domestic_labor_unions, beneficiary,
    organized, biographical, mobile, national).

% Benefit from government's retained fiscal space to fund social insurance, education, and public goods without international investors demanding austerity or portfolio withdrawal in response to expansionary spending. The constraint allows counter-cyclical domestic policy.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, welfare_state_constituencies, beneficiary,
    organized, generational, mobile, national).

% Cannot freely repatriate profits or move capital between subsidiaries across jurisdictions. Must accept that host governments retain authority to direct domestic credit and restrict foreign-exchange access. Must navigate licensing requirements, local-content rules, and restrictions on capital remittance that the Bretton Woods system legitimates.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, multinational_corporations, payer,
    powerful, biographical, constrained, global).

% Are structurally blocked by capital account restrictions and exchange controls. Hot money seeking arbitrage or panic-fleeing crises cannot move instantaneously across borders. The constraint is specifically designed to prevent precisely this flow type; currency speculators and short-term portfolio repositioning encounter active barriers enforced by central banks and exchange authorities.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, speculative_capital_flows, payer,
    powerful, immediate, trapped, global).

% Negotiate and enact the Articles of Agreement establishing the IMF and the rules of capital-account governance. Can amend the framework (as attempted in the 1960s–1970s) or withdraw from it. The institutional seat authoring the constraint itself.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_conference_signatories, agenda_setter,
    institutional, generational, mobile, global).

% Administers the capital-control rules, grants exceptions, provides liquidity facilities conditioned on compliance, and serves as the forum for dispute resolution. Interprets Article VI (restrictions on capital movements) and Article VIII (current-account convertibility) to balance member-state policy space against convertibility commitments.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_governance_structure, agenda_setter,
    institutional, generational, analytical, global).

% As the architect of Bretton Woods and holder of fixed gold parity (35 USD/oz), benefits from the stable-exchange-rate regime and from constraints on capital flight that protect dollar hegemony. The US dollar becomes the international reserve currency within a managed rather than free-floating system. Also retains policy space for domestic New Deal-style interventions.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, united_states_treasury, agenda_setter).

% The Soviet Union and planned-economy states remain outside the Bretton Woods system; their currencies are non-convertible and their capital markets are not integrated. They are excluded from the voting structure and cannot shape the rules, though the constraint's existence shapes their strategic position.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, competing_currency_blocs, excluded,
    powerful, generational, trapped, global).

% Newly independent nations gain formal authority to impose capital controls and direct domestic credit under the Bretton Woods framework, which legitimates policy tools they need to build infant industries and welfare states. However, they also enter an IMF voting system where industrial powers dominate governance, and their access to international liquidity becomes conditional on IMF approval—creating asymmetric leverage over their policy space.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postcolonial_nations, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, postcolonial_nations, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, national_governments).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rules-based international monetary order with fixed exchange rates and coordinated capital-account restrictions. Solves the coordination problem of how to enable international trade and investment while preventing the destructive capital flights and currency instability of the 1930s. Provides a framework for multilateral payments clearing and access to emergency liquidity (IMF credit facilities) rather than bilateral barter or competitive devaluations.
% TRANSFER_FUNCTION: Moves policy authority from international financial markets to national governments and multilateral institutions. Restricts the ability of capital to flow to highest-return jurisdictions, instead channeling it through official mechanisms. Transfers decision-making power over capital allocation from decentralized speculators to centralized national and international authorities. In this reading, the transfer protects domestic labor and welfare constituencies from capital-strike discipline.
% ABSENT_VOICES: Speculative international finance sector is structurally excluded from rule-setting; their preferences for free capital movement are present in IMF technical discussions but subordinate to member-state political authority. The interests of postcolonial nations are nominally included in IMF governance but vastly under-weighted by voting rules that reflect wartime-era power distribution. Capital markets themselves—as distributed agents—have no seat at the table.
% DISAPPEARANCE_RATIONALE: If Bretton Woods capital controls disappeared and were replaced with free capital movements, national governments would lose the policy autonomy the system protects. Monetary policy would immediately face arbitrage pressure; interest-rate differentials would trigger capital flows; welfare-state spending would face portfolio-withdrawal penalties; wage levels would face capital-flight threat when labor mobilizes. The entire post-war social democratic settlement—the embedding of markets within labor and welfare protections—depends on this constraint's persistence. The world rearranges into either a different regulatory regime or into the pre-Bretton-Woods competitive devaluation and capital-flight dynamics.
% FOUNDING_PROBLEM: The 1930s gold standard and unregulated capital flows produced synchronized global depression, competitive devaluations, and capital flights that bankrupted countries and workers. International finance became a mechanism of contagion rather than coordination. National governments needed policy space to counter-act the Depression and build welfare states, but the gold standard and free capital movement denied them that space. Bretton Woods was designed to restore international order while protecting that policy space.
% FOUNDING_PROBLEM_CORROBORATION: The Bretton Woods negotiators (Keynes, White, and the conference delegates) explicitly attested to the problem: the 1930s crisis was a coordination failure that required capital-account regulation to prevent recurrence. This reading is corroborated by post-war economic historians (Polanyi, Ruggie, Helleiner) and by the empirical record: the post-war period saw sustained growth, welfare-state expansion, and labor power precisely under the capital-control regime, whereas the periods of free capital movement (pre-1930s and post-1980s) saw greater financial instability and labor-market volatility. The neoliberal reading contests this, claiming the problem was government intervention rather than capital flight, but the empirical corroboration for the embedded-liberalism reading comes from outside the benefiting parties (academic economic history, labor movement testimony, postcolonial governments' assessments of policy autonomy).
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).
:- end_tests(bretton_woods_treaty_substrate__keynesian_embedded_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 in steady state) because the constraint serves a genuine coordination function (enabling multilateral trade, reducing competitive devaluation, preventing capital flights that destroyed 1930s economies) while also restricting the profit opportunities of international finance. The constraint is not pure extraction—governments genuinely benefit from the coordination, and the stability it enables supports long-term growth—but extractiveness is not negligible because speculative capital and financial intermediaries pay a real cost (reduced arbitrage opportunities, restricted returns, subordination to official channels). Extractiveness rises from 0.18 to 0.38 over the interval as (1) the system matures and capital controls are enforced more systematically (late 1940s–1960s); (2) financial pressure accumulates as US gold reserves decline and capital speculators probe the system's limits (1960s); (3) the constraint's extractive effects become clearer to finance-sector actors. It plateaus at 0.38 because the constraint's structure does not change—enforcement remains steady, alternatives remain constrained for financial capital, and the policy benefits for governments remain stable through the 1960s and into the early 1970s. Suppression is low (0.22) because the constraint's persistence does not primarily depend on coercion or hiding alternatives. National governments openly author capital-account restrictions; the IMF Articles explicitly permit them. Member states negotiate within the framework, and alternative arrangements (bilateral clearing agreements, informal currency unions, capital-flight precedent in the 1930s) remain vivid in memory. The suppression that exists is (a) the difficulty of exit for postcolonial nations and (b) the under-weighting of financial-sector preferences in IMF rule-making—not a hidden coercive apparatus. Theater is very low (0.12) because the constraint operates largely as stated: governments do impose capital controls; the IMF does enforce Article VI restrictions; the coordination function is real and acknowledged. The minimal theater reflects the constraint's structural authenticity—it does what it claims to do, unlike a snare that must disguise extraction as coordination.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (international finance, MNCs, speculative capital) and the beneficiary seats (national governments, labor, welfare constituencies) should compute very differently. From the beneficiary side, this is genuine rope: the constraint solves a real coordination problem (preventing 1930s-style contagion and capital flight) and enables the domestic policy autonomy that post-war social democracy depended on. From the payer side, especially the financial sector, the constraint appears more extractive: it restricts their ability to seek returns wherever capital is most productive; it subordinates market discovery to official channels; it limits arbitrage opportunities. The engine computes this divergence per seat from the structural data: beneficiary seats with mobile exit options and real coordination function derive d toward beneficiary (d near 0.0–0.3); payer seats with constrained exit and profit reduction derive d toward target (d near 0.7–1.0). This is exactly how seat divergence should operate: the same constraint feels like coordination to those who benefit from policy autonomy and like extraction to those who profit from financial mobility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: National governments (organized power, generational horizon, mobile exit options—they can theoretically exit Bretton Woods but choose to stay; exit was actually exercised by the UK in 1947 and discussed by others, but the framework's benefits kept most members committed) → derive d = 0.15–0.25 (beneficiaries toward the low end). Domestic labor and welfare constituencies (organized power but embedded within national governance; exit = emigration, high friction) → derive d = 0.2–0.3. Payers: International finance (powerful, biographical horizon, constrained exit—they cannot move to free-capital-movement regime without exiting international markets; this is a real trap) → derive d = 0.75–0.85. MNCs (powerful, biographical horizon, constrained exit—similar to finance) → derive d = 0.75–0.85. Speculative capital flows (powerful by financial volume, immediate time horizon, trapped—the constraint is explicitly designed to prevent this flow type; their exit option is nil) → derive d = 0.95 (nearly full target). Postcolonial nations (moderate power, generational horizon, constrained exit—they benefit from policy-space legitimation but are under-weighted in IMF governance) → derive d = 0.4–0.5 (closer to symmetric, pulled down by coordination benefit, pulled up by governance asymmetry). United States (institutional power, generational horizon, arbitrage exit—US can exit or reshape the system; gains enormous benefits from dollar hegemony and coordination stability) → derive d = 0.05–0.15 (strong beneficiary). IMF as administrator (institutional, analytical exit) → derive d = 0.5 (symmetric: coordinates the system but also enforces constraints on members).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy: the founding problem (1930s capital-flight contagion, competitive devaluation, synchronized depression, policy paralysis) remains live through the 1960s and beyond. Governments testify repeatedly that they need capital controls to maintain welfare systems. Labor movement and postcolonial governments attest to the founding problem's continuation: without capital controls, capital flight to higher-return jurisdictions would discipline domestic wages and welfare spending downward. The constraint persists because its founding function endures—it solves a coordination problem that, absent the constraint, would re-emerge. The measurement series shows extractiveness rising (late 1940s–1960s) as enforcement systematizes and as financial pressure accumulates, but this is NOT a sign of atrophied function; rather, it reflects maturation of the system and tightening of financial pressure as US gold reserves decline. The constraint remains functional, valued by beneficiaries, and actively maintained by IMF governance. Mandatrophy would show up as theater_ratio rising sharply (enforcement becoming performative) while the founding problem is independently solved or forgotten—neither is true here. Theater remains low because capital controls are still enforced as written and still serve their stated function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_vs_reading_boundary,
    'Is Bretton Woods a kernel (a stabilized commitment interpreted differently by different parties) or a constraint with a single structural meaning that different actors simply value differently?',
    'Historical analysis of the Articles of Agreement negotiation: did Keynes, White, and the signatories author the capital-control provisions with explicitly different understandings of their purpose, or did they reach consensus on the substantive structure and later readings diverge? Archival testimony, negotiation records, and the Articles'' interpretive history.',
    'If a genuine kernel, the three readings (embedded-liberalism, neoliberal, sovereignty-defense) are coordinate interpretations of a genuinely ambiguous text. If a single constraint with multiple valuations, the readings are ex post rationalizations of the same structure. This changes the classification: genuine kernel ambiguity produces reading_relations edges (coexists_with, forecloses, influences); simple valuation difference produces a single constraint with multiple seat perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_boundary, conceptual, 'Whether Bretton Woods is a kernel or a single constraint interpreted differently').

omega_variable(
    golden_age_causation,
    'Did the Bretton Woods constraint (capital controls) enable the post-war golden age of growth and welfare-state expansion, or did other factors (post-war demand, technological catch-up, labor power, full-employment policy) drive growth regardless of Bretton Woods structure?',
    'Counterfactual analysis: what would have happened under free capital movement? Comparison with historical alternatives (Triffin goldfix proposals, complete liberalization, Marshall Plan alternatives). Econometric estimation of welfare-state fiscal space enabled by capital controls vs. constrained by capital flight.',
    'If Bretton Woods-enabled growth is real, the constraint''s beneficiary status (governments, labor, welfare constituencies) is corroborated and extractiveness scores downward (more genuine coordination). If growth came from other factors, the constraint''s extraction component becomes more visible—it might be a snare disguised as coordination. This feeds mandatrophy evaluation: if the founding problem (preventing capital-flight discipline) was already solved by other conditions, the constraint becomes increasingly extractive and theatrical over time.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(golden_age_causation, empirical, 'Causal role of Bretton Woods capital controls in post-war growth and welfare-state expansion').

omega_variable(
    neoliberal_reading_contestation,
    'Does the neoliberal reading (Bretton Woods constrains government intervention to enable capital markets) genuinely foreclose the embedded-liberalism reading, or do they coexist within different institutional frames?',
    'Analyze whether both readings can be authored consistently with the same Articles of Agreement text. If both interpretations satisfy the text equally, they coexist; if one interpretation makes the other textually incoherent, there is foreclosure. Review post-1970s neoliberal reinterpretation efforts (IMF structural adjustment, capital-account liberalization push) as historical evidence of whether neoliberalism attempted to foreclose or merely to reweight existing provisions.',
    'If foreclosure: neoliberal reading is a later attempt to overturn embedded-liberalism within a single framework, which changes the reading_relations edge from coexists_with to forecloses. If coexistence: both readings remain live positions held by different institutional coalitions, which keeps the coexists_with edge. This affects cs_structure.reading_relations classification and the terminal attractor analysis for this constraint''s future state.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(neoliberal_reading_contestation, conceptual, 'Whether neoliberal and embedded-liberalism readings can coexist or whether one forecloses the other').

omega_variable(
    postcolonial_dual_positioning,
    'Are postcolonial nations genuinely positioned as beneficiaries (gaining policy-space legitimation through capital controls) or as victims (entering IMF governance with under-weighted voting power and conditional-access asymmetry)?',
    'Analyze postcolonial testimony and economic outcomes: do newly independent governments attest that capital controls enabled their policy autonomy, or do they report that IMF conditionality and governance asymmetry negated the capital-control benefit? Empirical record: did postcolonial nations successfully expand welfare states under Bretton Woods, or did they face IMF discipline that prevented expansion?',
    'If beneficiaries: the constraint genuinely coordinates their policy space, extractiveness is lower, and the constraint remains rope for this constituency. If victims: postcolonial nations are dual-positioned (benefits on paper, extraction in practice), extractiveness is higher for them, and the constraint may compute as tangled_rope at the postcolonial seat. This affects stakeholder directionality assignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_dual_positioning, empirical, 'Whether postcolonial nations benefited from policy-space legitimation or were trapped by IMF asymmetry').

omega_variable(
    embedded_liberalism_decomposition_candidate,
    'Should this constraint be decomposed into two separate stories: (1) Bretton Woods capital-controls regime (benefits national governments and labor, constrains finance), and (2) IMF governance asymmetry (governance weights shifted toward industrial powers, conditional access for postcolonial nations)?',
    'ε-invariance test: does measuring the constraint via capital controls alone vs. via IMF governance structure produce significantly different extractiveness values? If yes, decompose into two stories per OQ-84 protocol. If no, the unified story captures the constraint''s structural essence.',
    'If decomposed: two stories with different beneficiary sets, different victim sets, different claimed types, and different ε values. Story 1 (capital controls) = moderate rope. Story 2 (IMF governance) = higher-extraction tangled_rope or snare. Linked via network.affects_constraints. If unified: the dual positioning of postcolonial nations (beneficiary from controls, victim from governance) becomes a complex seat profile that the engine must resolve.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embedded_liberalism_decomposition_candidate, conceptual, 'Whether capital-control coordination and IMF-governance asymmetry should be separate constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(bret_tr_t0, observed).
narrative_ontology:measurement(bret_tr_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 5, 0.09).
narrative_ontology:measurement_basis(bret_tr_t5, observed).
narrative_ontology:measurement(bret_tr_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 10, 0.1).
narrative_ontology:measurement_basis(bret_tr_t10, observed).
narrative_ontology:measurement(bret_tr_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 15, 0.11).
narrative_ontology:measurement_basis(bret_tr_t15, observed).
narrative_ontology:measurement(bret_tr_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(bret_tr_t20, observed).
narrative_ontology:measurement(bret_tr_t25, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 25, 0.12).
narrative_ontology:measurement_basis(bret_tr_t25, observed).
narrative_ontology:measurement(bret_tr_t30, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 30, 0.12).
narrative_ontology:measurement_basis(bret_tr_t30, observed).
narrative_ontology:measurement(bret_tr_t35, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 35, 0.12).
narrative_ontology:measurement_basis(bret_tr_t35, observed).
narrative_ontology:measurement(bret_tr_t40, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(bret_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bret_be_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(bret_be_t0, observed).
narrative_ontology:measurement(bret_be_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(bret_be_t5, observed).
narrative_ontology:measurement(bret_be_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(bret_be_t10, observed).
narrative_ontology:measurement(bret_be_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 15, 0.32).
narrative_ontology:measurement_basis(bret_be_t15, observed).
narrative_ontology:measurement(bret_be_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(bret_be_t20, observed).
narrative_ontology:measurement(bret_be_t25, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 25, 0.37).
narrative_ontology:measurement_basis(bret_be_t25, observed).
narrative_ontology:measurement(bret_be_t30, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(bret_be_t30, observed).
narrative_ontology:measurement(bret_be_t35, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(bret_be_t35, observed).
narrative_ontology:measurement(bret_be_t40, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(bret_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t0, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(bret_su_t0, observed).
narrative_ontology:measurement(bret_su_t5, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(bret_su_t5, observed).
narrative_ontology:measurement(bret_su_t10, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(bret_su_t10, observed).
narrative_ontology:measurement(bret_su_t15, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 15, 0.2).
narrative_ontology:measurement_basis(bret_su_t15, observed).
narrative_ontology:measurement(bret_su_t20, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(bret_su_t20, observed).
narrative_ontology:measurement(bret_su_t25, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(bret_su_t25, observed).
narrative_ontology:measurement(bret_su_t30, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(bret_su_t30, observed).
narrative_ontology:measurement(bret_su_t35, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 35, 0.22).
narrative_ontology:measurement_basis(bret_su_t35, observed).
narrative_ontology:measurement(bret_su_t40, bretton_woods_treaty_substrate__keynesian_embedded_liberalism, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(bret_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__neoliberal_convertibility).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, bretton_woods_treaty_substrate__sovereignty_defense).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, imf_governance_asymmetry_postcolonial_nations).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, gold_standard_constraint_alternative).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Bretton Woods kernel. The sibling readings (neoliberal_convertibility and sovereignty_defense) instantiate different interpretations of the same treaty. All three stories share the same referent (the Article VI capital-control provisions and fixed exchange-rate regime) but assign different beneficiary/victim structures based on their reading of what Bretton Woods was designed to accomplish. The embedded-liberalism reading emphasizes the protection of domestic policy autonomy for national governments and labor constituencies. The neoliberal reading emphasizes the enabling of capital markets and eventual liberalization. The sovereignty-defense reading emphasizes the preservation of national monetary sovereignty against external discipline. These are not three different constraints; they are three readings of one kernel, each with its own ε value, beneficiary set, and claimed type. The ε values differ because the readings assess the standing arrangement differently: embedded-liberalism reads capital controls as legitimate coordination (moderate extraction from finance, genuine benefit to governments); neoliberal reads them as temporary interference with market efficiency (higher extraction from markets, illegitimate constraint on growth); sovereignty-defense reads them as asymmetric protection (moderate extraction from external creditors, benefit to national policy authority). Linked via network.affects_constraints to show the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__keynesian_embedded_liberalism, powerful, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
