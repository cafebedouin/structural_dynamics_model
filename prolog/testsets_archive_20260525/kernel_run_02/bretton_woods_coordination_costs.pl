% ============================================================================
% CONSTRAINT STORY: bretton_woods_coordination_costs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_coordination_costs, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bretton_woods_coordination_costs
 *   human_readable: Bretton Woods Coordination Costs and the Nixon Shock Collapse
 *   domain: monetary_economics/international_political_economy
 *
 * SUMMARY:
 *   The Bretton Woods monetary system (1944-1971) represents one of history's
 *   largest coordination mechanisms: 44 nations fixing exchange rates to the
 *   US dollar, which itself is fixed to gold at $35/ounce. The system
 *   generated genuine coordination benefits — predictable trade, reduced
 *   currency speculation, capital accumulation — while extracting asymmetric
 *   rents to the dollar center. The 1971 Nixon Shock collapse presents a
 *   structural puzzle: was it a discrete policy reversal or an overdetermined
 *   composite of incompatible constraints (the Triffin dilemma, US current
 *   account deficits, and gold reserve depletion) that made collapse
 *   inevitable? This constraint examines the underlying mechanism: Bretton
 *   Woods functioned as a tangled rope (genuine coordination WITH asymmetric
 *   extraction) whose extraction costs rose over time as the Triffin dilemma
 *   tightened, eventually making the peg unsustainable. The constraint's
 *   theater ratio (0.48) reflects that the explicit gold commitment became
 *   increasingly performative in the 1960s — the gold pool mechanism and the
 *   special drawing rights innovation were patches designed to maintain the
 *   fiction of backing when actual reserves could not support the dollar
 *   liabilities. The extractiveness trajectory (0.22 to 0.58) shows rising
 *   extraction as coordination became less voluntary and more coercive,
 *   culminating in the collapse.
 *
 * KEY AGENTS:
 *   - US Treasury and Federal Reserve: Primary beneficiary (institutional/arbitrage) — architect of system, captures seigniorage, chooses exit timing and terms
 *   - Sterling Bloc Nations (UK, Australia, Canada, India): Primary victim (powerless/trapped) — fixed to sterling which is pegged to dollar; dependent on trade coordination; face devaluation shocks with no exit options
 *   - Western European Central Banks (France, Germany, Netherlands): Secondary actor (powerful/constrained) — hold dollar reserves, constrained by international obligations, benefit from dollar as trade numeraire, but gain agency through European alternatives (EMS)
 *   - Fixed-Exchange Periphery (Japan, developing nations): Victim (powerless to moderate/trapped) — locked into dollar pegs with no adjustment mechanism for US deficits
 *   - Global Trade Coordination (abstract collective): Victim (powerless/trapped) — the system's genuine coordination function, which breaks down when asymmetric extraction becomes visible
 *   - International Monetary Establishment (IMF, World Bank, OECD): Organized actor (organized/constrained) — designed Bretton Woods with planned sunset; manages transition to post-1971 regimes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices as mathematical inevitability via Triffin dilemma
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_coordination_costs, 0.58).
domain_priors:suppression_score(bretton_woods_coordination_costs, 0.52).
domain_priors:theater_ratio(bretton_woods_coordination_costs, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_coordination_costs, extractiveness, 0.58).
narrative_ontology:constraint_metric(bretton_woods_coordination_costs, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bretton_woods_coordination_costs, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_coordination_costs, tangled_rope).
narrative_ontology:human_readable(bretton_woods_coordination_costs, "Bretton Woods Coordination Costs and the Nixon Shock Collapse").
narrative_ontology:topic_domain(bretton_woods_coordination_costs, "monetary_economics/international_political_economy").

domain_priors:requires_active_enforcement(bretton_woods_coordination_costs).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(bretton_woods_coordination_costs, formalized).
narrative_ontology:cs_authority_grounding(bretton_woods_coordination_costs, lineage).
narrative_ontology:cs_interpretation_layer_present(bretton_woods_coordination_costs).
narrative_ontology:cs_reading_relation(bretton_woods_coordination_costs, bretton_woods_inevitable_collapse, coexists_with).
narrative_ontology:cs_reading_relation(bretton_woods_coordination_costs, bretton_woods_designed_sunset, coexists_with).
narrative_ontology:cs_axiom(bretton_woods_coordination_costs, foundational, discretionary_policy_choice).
narrative_ontology:cs_axiom_status(discretionary_policy_choice, holdable).
narrative_ontology:cs_axiom_grounding(bretton_woods_coordination_costs, discretionary_policy_choice, empirically_contingent).
narrative_ontology:cs_axiom(bretton_woods_coordination_costs, secondary, coordination_mechanism_primary).
narrative_ontology:cs_axiom_status(coordination_mechanism_primary, holdable).
narrative_ontology:cs_axiom_grounding(bretton_woods_coordination_costs, coordination_mechanism_primary, instrumental).
narrative_ontology:cs_reference_frame(bretton_woods_coordination_costs, bretton_woods_as_negotiated_settlement).
narrative_ontology:cs_drift_state(bretton_woods_coordination_costs, post_1968_gold_pool_exhaustion, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_coordination_costs, us_monetary_autonomy).
narrative_ontology:constraint_beneficiary(bretton_woods_coordination_costs, reserve_currency_issuers).
narrative_ontology:constraint_victim(bretton_woods_coordination_costs, sterling_bloc_nations).
narrative_ontology:constraint_victim(bretton_woods_coordination_costs, fixed_exchange_periphery).
narrative_ontology:constraint_victim(bretton_woods_coordination_costs, global_trade_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(bretton_woods_coordination_costs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bretton_woods_coordination_costs, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(bretton_woods_coordination_costs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bretton_woods_coordination_costs, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bretton_woods_coordination_costs, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(bretton_woods_coordination_costs, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_coordination_costs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bretton_woods_coordination_costs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bretton_woods_coordination_costs, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_coordination_costs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bretton_woods_coordination_costs, TR),
    TR >= 0.70.

:- end_tests(bretton_woods_coordination_costs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over the 26-year interval. Initial extractiveness (0.22) reflects genuine coordination benefits without proportional costs — the system truly solved the inter-war coordination catastrophe. By 1968 (point 16), extractiveness rises to 0.52 as US dollar supply growth outpaces gold reserves, and the system becomes increasingly extractive: peripheral nations must accept currency devaluation rather than adjust US policy. By 1971 (point 26), extractiveness peaks at 0.58 as the peg becomes purely performative and collapse is imminent. Suppression (0.52): Moderate. Bretton Woods includes capital controls and fixed exchange rates that suppress adjustment alternatives for member nations, but the system is not a total lock-in — nations can exit (at high cost) or renegotiate (with difficulty). Theater ratio (0.48): Moderate. The explicit gold commitment is backed by actual US reserves through much of the period, making it more than pure theater. However, by the 1960s, the gold pool and SDR mechanisms are explicitly theatrical — patches designed to maintain the fiction of full backing when reserves are plainly insufficient. The theater ratio rises as the system ages, reflecting growing gap between stated commitment (convertibility) and actual capability (gold depletion).
 *
 * PERSPECTIVAL GAP:
 *   This constraint spans the full range from Snare (trapped peripheral nations) to Rope (US beneficiary) to Scaffold (international establishment's planned sunset) to Piton (gold-backing mythology) to Mountain (natural law via Triffin dilemma). The perspectival gap reveals that 'Bretton Woods collapse' is not a single event but multiple events depending on observer position. For the trapped nations, it is a snare breaking down through exhaustion. For the US, it is a rope artifact abandoned when extraction becomes too visible. For the international establishment, it is a planned scaffold sunset. For the analytical observer, it appears as an inevitable mountain — but this naturalization obscures that the constraint was constructed and that beneficiaries actively maintained it through theatrical mechanisms (gold pool, SDR) when pressures mounted. The gap between the mountain view and the tangled-rope reality is diagnostic of false-summit naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Bretton Woods creates a directed extraction flow FROM peripheral nations and the sterling bloc (high d) TO the US center (low d). The beneficiary's arbitrage options mean they experience low chi; the trapped victims experience high chi. The intervening institutional actors have constrained options that place them at intermediate d values. The analytical observer's canonical d (≈0.72) is pulled upward by the powerless conceptual position of 'global coordination' — but this masks that the observer can actually analyze the system and see its constructed character. The identity_locked perspective would apply to nations that have internalized dollar dependence as inevitable and natural (many post-colonial states, some peripheral economies) — they have structural options (capital account liberalization, currency union alternatives) but cannot see these options because their institutional identity is fused with dollar-based regimes. See omega_policy_discretion_vs_overdetermination for the framing dispute on whether this structural position is inevitable or contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   Bretton Woods resolves the mandatrophy by showing that a single historical mechanism instantiates multiple constraint types depending on agent perspective and time horizon. The US experiences it as Rope (coordination with benefits). Trapped nations experience it as Snare. The international establishment designed it as Scaffold. The gold commitment became Piton (theatrical maintenance). The analytical observer risks seeing it as Mountain (natural law impossibility). NO SINGLE CLASSIFICATION is 'correct' — the presheaf of perspectives over the observation site is the answer. The mandatrophy is also resolved by recognizing that 'Bretton Woods' may label two distinct constraints: (1) the coordination mechanism itself (genuinely rope-like in the immediate horizon), and (2) the gold-to-fiat transition path (tangled-rope with rising extraction). The decomposition would place these in separate stories with different ε values and network links. The current story treats them as one constraint to highlight the perspectival dissolution of the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    policy_discretion_vs_overdetermination,
    'Was the 1971 collapse a discrete policy choice by Nixon (contingent actor discretion) or an overdetermined inevitable event (structural logic)?',
    'Counterfactual analysis of alternative exit paths (Bretton Woods II with SDR numeraire, gold price adjustment without full peg abandonment, partial coordination zones); examination of decision-maker communications during 1968-1971 to assess perceived alternatives; analysis of whether the Triffin dilemma truly foreclosed all intermediate solutions or whether policy makers simply chose the most advantageous exit path.',
    'If discretion-dominant: classify as snare/tangled_rope (constructed extraction mechanism) with sunset logic under organized actor pressure. If overdetermined-dominant: classify as mountain (natural law of incompatible constraints) with false-summit overlay. The truth likely sits between: Triffin logic created structural pressures (mountain substrate), but multiple exit paths existed and US selected the one maximizing seigniorage (tangled_rope with beneficiary discretion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_discretion_vs_overdetermination, conceptual, 'Whether collapse was policy choice or structural overdetermination').

omega_variable(
    trilemma_versus_specific_choices,
    'Does the monetary trilemma (impossible trinity: simultaneous independent monetary policy, fixed exchange rates, and free capital flows) fully explain Bretton Woods collapse, or does it naturalize contingent choices about capital account liberalization and peg rigidity?',
    'Historical analysis of post-WWII capital controls and their role in maintaining the gold peg; examination of whether selective capital account restrictions could have extended fixed-rate viability; comparison with post-1971 EMS, which maintained fixed rates (partially) by restricting capital flows; identification of the specific moment when ''peg abandonment'' became politically easier than ''capital control maintenance''.',
    'If trilemma fully explains: mountain classification correct (structural law of macroeconomics). If contingent choice overlay exists: tangled_rope classification better captures that organized actors selected among policy packages, choosing to liberate capital flows rather than maintain controls or adjust pegs. The Bretton Woods system WAS sustainable under continued capital controls — the collapse reflects choice to prioritize capital account liberalization, not logical inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trilemma_versus_specific_choices, empirical, 'Whether impossible trinity or capital liberalization choice drove collapse').

omega_variable(
    beneficiary_intent_obscuration,
    'Did the US explicitly construct the Bretton Woods system with advance knowledge of the Triffin dilemma and a planned 20-year extraction window, or was the dilemma unanticipated?',
    'Examination of Keynes-White debates and early IMF architecture documents for evidence of anticipated sustainability horizons; analysis of US Treasury strategy memos from 1945-1950 revealing expected lifespan; comparison of how confidently policymakers projected the gold commitment forward (permanent rhetoric vs. provisional framing); assessment of whether the 1960 London Gold Pool was a patch or a planned extraction extension.',
    'If known in advance: Bretton Woods becomes deliberately designed tangled_rope (coordinated extraction mechanism with beneficiary advantage built in). If unanticipated: becomes snare (genuine coordination mechanism that degraded into extraction through market pressures). Beneficiary knowledge changes the moral and structural character fundamentally — intentional extraction vs. emergent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intent_obscuration, empirical, 'Whether US deliberately engineered time-limited extraction window').

omega_variable(
    kernel_reading_contingency,
    'Is this constraint one reading of a contested kernel (the Bretton Woods institutional settlement itself), where different parties held different understandings of the commitment''s permanence and adjustment mechanisms?',
    'Documentation of founder intentions (Keynes vs. White divergence on currency reserves vs. gold backing, conditional vs. unconditional liquidity commitments); analysis of how different nations interpreted the Articles of Agreement; examination of whether ''gold peg permanence'' was truly the agreed kernel or whether flexibility was always understood.',
    'If kernel reading applies: this constraint is the single-event collapse reading competing with a multi-pressure-composite reading. Different framings of the settlement''s original commitment structure lead to different narratives of 1971: dramatic policy reversal vs. return to planned structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether Bretton Woods collapse is kernel reading contest').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_coordination_costs, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bwcc_tr_t0, bretton_woods_coordination_costs, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bwcc_tr_t8, bretton_woods_coordination_costs, theater_ratio, 8, 0.28).
narrative_ontology:measurement(bwcc_tr_t16, bretton_woods_coordination_costs, theater_ratio, 16, 0.44).
narrative_ontology:measurement(bwcc_tr_t26, bretton_woods_coordination_costs, theater_ratio, 26, 0.48).

% Extraction over time
narrative_ontology:measurement(bwcc_be_t0, bretton_woods_coordination_costs, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bwcc_be_t8, bretton_woods_coordination_costs, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(bwcc_be_t16, bretton_woods_coordination_costs, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(bwcc_be_t26, bretton_woods_coordination_costs, base_extractiveness, 26, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_coordination_costs, resource_allocation).
narrative_ontology:affects_constraint(bretton_woods_coordination_costs, triffin_dilemma_macroeconomic_trilemma).
narrative_ontology:affects_constraint(bretton_woods_coordination_costs, dollar_hegemony_asymmetric_seigniorage).
narrative_ontology:affects_constraint(bretton_woods_coordination_costs, post_bretton_woods_floating_instability).

% DUAL FORMULATION NOTE:
% Bretton Woods coordination costs can be decomposed into three structurally distinct constraints: (1) the genuine coordination function (ε≈0.25, Rope) — enabling predictable trade and capital accumulation; (2) the asymmetric extraction mechanism (ε≈0.55, Tangled Rope) — US seigniorage and adjustment asymmetry; (3) the inevitable collapse mechanism (ε≈0.08, Mountain or overdetermination hypothesis). The current story treats these as one constraint to highlight perspectival differences. Decomposition into separate stories would be justified if detailed historical analysis distinguishes how much of the 1971 collapse was coordination failure vs. beneficiary discretion vs. mathematical impossibility. The network edges link to the Triffin dilemma (upstream macroeconomic constraint), dollar hegemony extraction (alternative framing of the same mechanism), and post-1971 floating regimes (downstream institutional consequence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_coordination_costs, institutional, 0.18).
constraint_indexing:directionality_override(bretton_woods_coordination_costs, powerless, 0.91).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
