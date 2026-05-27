% ============================================================================
% CONSTRAINT STORY: punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_punctuated_swap_reading, []).

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
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: punctuated_swap_reading
 *   human_readable: Punctuated Swap Reading: Nixon Shock as Institutional Coordination Failure
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   On August 15, 1971, President Richard Nixon announced the end of U.S.
 *   dollar convertibility to gold at the fixed rate of $35/oz, terminating
 *   the Bretton Woods system in a discrete institutional act. This reading
 *   interprets that moment as a punctuated swap: a deliberate institutional
 *   choice by U.S. authorities to escape the coordination constraints of a
 *   fixed-peg system. The constraint exhibits the structure of a coordination
 *   failure solved via unilateral defection. The U.S. benefited from
 *   restoration of monetary policy autonomy; foreign dollar holders suffered
 *   expropriation via effective revaluation; allied governments faced
 *   disrupted trade and capital patterns; financial markets experienced
 *   arbitrage opportunities; and a civilizational observer risks naturalizing
 *   the choice as inevitable structural collapse rather than institutional
 *   decision. This reading is one of three competing interpretations of the
 *   contested kernel 'monetary_anchor_principle.' The sibling readings
 *   interpret August 15 as either an inevitable consequence of the Triffin
 *   Dilemma (triffin_inevitability_reading) or as an overdetermined composite
 *   of multiple independent institutional pressures
 *   (overdetermined_composite_reading).
 *
 * KEY AGENTS:
 *   - Nixon Administration / U.S. Treasury: Primary beneficiary (powerful/mobile) — restores fiscal autonomy by unilateral exit from gold convertibility commitment
 *   - Foreign Central Banks and Dollar Holders: Primary victim (powerless/trapped) — suffer expropriation via effective devaluation; no exit capacity during shock
 *   - Allied Governments (France, Germany, UK, Canada): Secondary beneficiary/victim (organized/constrained) — constrained to accept terms through U.S. political weight; later benefit from monetary independence but suffer immediate trade/capital disruption
 *   - Financial Markets and Arbitrageurs: Institutional observers (institutional/arbitrage) — experience the shock as coordination problem and trading opportunity; high mobility
 *   - Developing Economies and Non-Reserve Currency Nations: Tertiary victims (moderate/constrained) — dependent on dollar credit and export earnings; face terms-of-trade deterioration
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the discrete choice as structural inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(punctuated_swap_reading, 0.38).
domain_priors:suppression_score(punctuated_swap_reading, 0.42).
domain_priors:theater_ratio(punctuated_swap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(punctuated_swap_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(punctuated_swap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(punctuated_swap_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(punctuated_swap_reading, tangled_rope).
narrative_ontology:human_readable(punctuated_swap_reading, "Punctuated Swap Reading: Nixon Shock as Institutional Coordination Failure").
narrative_ontology:topic_domain(punctuated_swap_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(punctuated_swap_reading, '4098267e-294c-4cbb-8dfe-f1af9428190c').
narrative_ontology:cs_created_at('4098267e-294c-4cbb-8dfe-f1af9428190c', '').
narrative_ontology:cs_kernel_codification('4098267e-294c-4cbb-8dfe-f1af9428190c', fixed_text).
narrative_ontology:cs_authority_grounding('4098267e-294c-4cbb-8dfe-f1af9428190c', extraction).
narrative_ontology:cs_interpretation_layer_present('4098267e-294c-4cbb-8dfe-f1af9428190c').
narrative_ontology:cs_kernel_id(punctuated_swap_reading, monetary_anchor_principle).
narrative_ontology:cs_reading_relation('4098267e-294c-4cbb-8dfe-f1af9428190c', triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_reading_relation('4098267e-294c-4cbb-8dfe-f1af9428190c', overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('4098267e-294c-4cbb-8dfe-f1af9428190c', foundational, institutional_agency_sovereign_choice).
narrative_ontology:cs_axiom_status(institutional_agency_sovereign_choice, holdable).
narrative_ontology:cs_axiom_grounding('4098267e-294c-4cbb-8dfe-f1af9428190c', institutional_agency_sovereign_choice, instrumental).
narrative_ontology:cs_axiom('4098267e-294c-4cbb-8dfe-f1af9428190c', secondary, unilateral_defection_extractive).
narrative_ontology:cs_axiom_status(unilateral_defection_extractive, holdable).
narrative_ontology:cs_axiom_grounding('4098267e-294c-4cbb-8dfe-f1af9428190c', unilateral_defection_extractive, deontological).
narrative_ontology:cs_reference_frame('4098267e-294c-4cbb-8dfe-f1af9428190c', bretton_woods_fixed_commitment).
narrative_ontology:cs_drift_state('4098267e-294c-4cbb-8dfe-f1af9428190c', contemporary_institutional_analysis_era, gap(practice_drift, substantial, true)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(punctuated_swap_reading, us_fiscal_autonomy).
narrative_ontology:constraint_victim(punctuated_swap_reading, foreign_dollar_holders).
narrative_ontology:constraint_victim(punctuated_swap_reading, bretton_woods_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: U.S. AUTHORITIES (TANGLED ROPE) — Powerful institutional actors with mobility; the constraint operates as coordination failure solved via unilateral defection. The U.S. benefits from monetary autonomy (escapes gold convertibility obligation) while bearing the cost of legitimacy damage. Immediate time horizon reflects the discrete August 15 decision. Extraction is present (expropriation of foreign dollar reserves via devaluation) but paired with genuine coordination function (restoring domestic monetary policy independence). Active enforcement required to maintain the new regime.
constraint_indexing:constraint_classification(punctuated_swap_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: FOREIGN DOLLAR HOLDERS (SNARE) — Trapped in dollar dependency; face sudden expropriation via effective devaluation (21% revaluation against gold equivalent). No exit capacity during the immediate shock. Biographical time horizon reflects the generational commitment foreign governments made to dollar-denominated reserves. Extraction is severe — a unilateral institutional choice imposed costs retroactively on foreign actors who had aligned their monetary policy with the Bretton Woods commitment. Suppression is high: alternative reserve currencies insufficient to absorb reserve flows; forced holding of depreciating assets.
constraint_indexing:constraint_classification(punctuated_swap_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DEVELOPING ECONOMIES (SNARE) — Moderate power but constrained exit; dependent on dollar-denominated international credit and export earnings. The shock disrupted trade patterns, inflation dynamics, and capital flows. Developing nations could not devalue as aggressively as the U.S. without capital flight or debt defaults. Generational time horizon reflects multi-year adjustment period. Extraction occurs through terms-of-trade deterioration and capital flow disruption — costs imposed by the unilateral U.S. choice with no consultation.
constraint_indexing:constraint_classification(punctuated_swap_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: FINANCIAL MARKETS (ROPE) — Institutional actors with arbitrage capacity; the shock created massive trading opportunities. Gold price floated upward; currencies realigned; relative pricing dislocations enabled profitable arbitrage. The markets experience the constraint primarily as a coordination problem (how to price assets in a new regime) rather than as extraction. Active trading mechanisms rapidly established new exchange rate equilibria. Low suppression for these actors — mobility is high.
constraint_indexing:constraint_classification(punctuated_swap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALLIED GOVERNMENTS (TANGLED ROPE) — Organized actors (France, Germany, UK, Canada) with constrained exit; allied to the U.S. by treaty but economically disadvantaged by the shock. These governments could retaliate through trade measures or by accelerating European monetary integration (which they did — precursor to EMU). The constraint exhibits mixed coordination and extraction: the U.S. solved its own coordination problem (restoring monetary autonomy) while imposing costs on allied economies. Active enforcement through U.S. political pressure maintained the new regime against allied resistance.
constraint_indexing:constraint_classification(punctuated_swap_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FIXED COMMITMENT VIEW (MOUNTAIN) — From a civilizational perspective, the Bretton Woods framework created an immutable commitment problem: a monetary arrangement where the anchor currency (U.S. dollar) is also a national fiat currency creates inherent instability. Once the U.S. experienced fiscal pressure (Vietnam War spending, Great Society), the mathematical constraints on maintaining fixed-exchange gold conversion became impossible — not a choice but an inevitable structural collapse. This perspective naturalizes the August 15 decision as an unavoidable consequence of conflicting commitments embedded in the Bretton Woods architecture. Engine false summit detection applies: the decision WAS discrete (institutional choice on August 15), and the structural data contradicts mountain classification.
constraint_indexing:constraint_classification(punctuated_swap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(punctuated_swap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(punctuated_swap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(punctuated_swap_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.38): Moderate. The U.S. choice extracted value from foreign dollar holders through effective revaluation (21% devaluation against gold equivalent), but the extraction is not as severe as a pure snare (which would show ε ≥ 0.46) because the shift also solved a genuine coordination problem — the Bretton Woods system had become structurally unsustainable given conflicting monetary and fiscal commitments. The extractiveness value reflects that the institutional choice WAS self-interested (U.S. fiscal autonomy) AND functional (restoring monetary policy effectiveness). If the choice had been purely extractive with no coordination problem-solving, extractiveness would be higher. If the shift had been purely functional with no self-interest component, extractiveness would be lower. The moderate value captures the hybrid: both coordination failure solved AND unilateral defection. Suppression (0.42): Moderate. Foreign actors faced significant barriers to exit (currency dependency, trade integration, capital controls), but suppression was not total — alternative reserve arrangements (SDRs, gold repatriation, European monetary integration) became feasible once the U.S. unilaterally terminated convertibility. The shock created a brief period of maximum suppression (days 1-3) followed by rapid institutional innovation to reduce suppression (floating regimes, swap facilities). Theater ratio (0.35): Low-moderate. The actual institutional change was straightforward — a Treasury decision and announcement — with minimal performative content. However, subsequent negotiations and legitimacy-building (Smithsonian Agreement negotiations, currency revaluation announcements) added theatrical elements. The pre-shock period had theater (gold-standard defense rituals, liquidity crisis management theater), while the immediate post-shock period focused on functional market adjustment.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between powerful-actor-with-agency (U.S. authorities seeing coordination problem and solution) and trapped-victims (foreign holders seeing sudden expropriation). The U.S. perspective classifies the constraint as tangled_rope or rope — genuine coordination failure paired with self-interested institutional choice, but functional outcome. The foreign holder perspective classifies as snare — pure extraction with suppression and no coordination benefit to them. The analytical observer risks misclassifying as mountain — naturalizing the institutional choice as inevitable structural law rather than deliberate decision. The allied government perspective occupies the middle ground: organized enough to contest (they did, through European monetary integration), but constrained enough to accept terms (constrained by security alliance, trade dependence, capital controls). The market perspective sees rope — a coordination problem (how to price assets) being solved through rapid equilibrium-finding, with minimal suppression for actors with arbitrage capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for this constraint derives from sharp beneficiary/victim declarations combined with differentiated power and exit capacity. The U.S. authorities occupy (powerful, mobile, institutional, arbitrage) — they are the beneficiary with maximum agency and can escape the constraint they created by moving to a fiat system. Their d ≈ 0.10-0.15 (slight beneficiary position, but one they engineered). Foreign dollar holders occupy (powerless/moderate, trapped/constrained, variable power, immediate time horizon) — they are victims caught in the constraint imposed on them. Their d ≈ 0.80-0.95 depending on exit options. The sigmoid f(d) amplifies this asymmetry: beneficiaries experience dampened effective extraction (f(d) ≈ -0.05 at d=0.15), while victims experience amplified effective extraction (f(d) ≈ 1.35 at d=0.90). This accounts for why the same structural arrangement feels like coordination success (U.S. perspective) and predation (foreign holder perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy dissolves once the reading declares this constraint as a coordinate problem PLUS unilateral defection (tangled_rope), not a false choice between pure coordination (rope) and pure extraction (snare). The false dilemma was: 'Either Bretton Woods worked perfectly as coordination (it didn't — Triffin) or it was pure U.S. exploitation (it wasn't — U.S. benefited from the system too, until it didn't). The actual structure is tangled_rope: the system genuinely solved coordination problems in the post-WWII period AND embedded asymmetric extraction (seigniorage for the U.S., vulnerability for foreign holders). August 15 escalated the asymmetry by unilateral decision, but it didn't create the underlying asymmetry — that was inherent to the Bretton Woods design. The mandatrophy is resolved by distinguishing the institutional arrangement (tangled_rope throughout 1944-1971) from the institutional choice (August 15 decision, which is the extraction moment). The choice was discrete and self-interested; the constraint was hybrid throughout.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discretion_vs_inevitability,
    'Was August 15, 1971 a discrete institutional choice by Nixon and Kissinger, or an inevitable punctuation point in structural decomposition of Bretton Woods?',
    'Counterfactual analysis: what alternative paths existed? (gold-standard defense via capital controls, negotiated regime transition, earlier dollar abandonment). Historical record of policy deliberations and considered alternatives. Comparison with other fixed-peg collapses (Britain 1931, ERM 1992).',
    'If choice: classification is tangled_rope (coordination failure solved by unilateral defection); extractiveness ≈ 0.38. If inevitable: classification shifts toward rope (coordination problem solved, even though painfully); extractiveness ≈ 0.25. This omega directly determines the reading assignment — punctuated_swap vs inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_vs_inevitability, conceptual, 'Whether the August 15 decision was a discrete choice or structural inevitability').

omega_variable(
    beneficiary_expropriation_boundary,
    'Does U.S. restoration of fiscal autonomy constitute legitimate policy correction or expropriation of foreign dollar holders?',
    'Comparative institutional analysis: Were there compensation mechanisms available (special drawing rights, currency swap facilities, reparations for exchange-rate losses)? Did the U.S. offer negotiated transition versus unilateral imposition? Historical precedent for how anchor currency crises are managed. Counterfactual comparison with negotiated regime change scenarios.',
    'If legitimate correction: beneficiary/victim distinction is more ambiguous; extraction is lower; classification may shift toward rope. If expropriation: beneficiary/victim distinction is sharp; extraction is higher; tangled_rope classification confirmed. This omega determines the moral framing of directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_expropriation_boundary, preference, 'Whether U.S. policy shift constitutes legitimate autonomy or expropriation').

omega_variable(
    regime_permanence_assumption,
    'Did the August 15 decision establish a genuinely stable new regime (fiat currency system), or was it a transition point to a future re-anchoring?',
    'Empirical: Has fiat currency system persisted for 50+ years (yes, as of 2026). Counterfactual: What would gold-standard re-anchoring at higher price have meant? Analysis of whether SDR-based regime or commodity-basket anchor were live alternatives. Comparison with other currencies that attempted fiat→anchor transitions.',
    'If stable: scaffold sunset clause is irrelevant — no return path to Bretton Woods was engineered. If transitional: the punctuated swap might be reframed as temporary scaffolding toward a different anchor. Current evidence strongly supports stability, so this omega is low-impact but conceptually clarifying.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_permanence_assumption, empirical, 'Whether the fiat regime shift was permanent or transitional').

omega_variable(
    committer_kernel_framing,
    'This constraint is one reading of the contested kernel ''monetary_anchor_principle.'' Which alternative framing of the kernel — as inevitability (triffin_inevitability_reading) or overdetermined institutional composite (overdetermined_composite_reading) — most accurately captures what August 15 was?',
    'Comparative constraint analysis: Does the inevitability reading produce empirically consistent classifications across all perspectives? Does the overdetermined composite reading successfully decompose August 15 into multiple independent structural pressures? Which reading minimizes classification gaps and maximizes explanatory power?',
    'If inevitability reading dominates: punctuated_swap reading should reclassify as downstream/secondary. If overdetermined composite dominates: punctuated_swap becomes one component of a larger institutional failure. If punctuated_swap dominates: the discrete choice framing is legitimate and the sibling readings are perspectival variations on the same constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_framing, conceptual, 'Which reading of the monetary_anchor_principle kernel is structurally dominant').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(punctuated_swap_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_preannounce, punctuated_swap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(theater_shock_moment, punctuated_swap_reading, theater_ratio, 1, 0.35).
narrative_ontology:measurement(theater_post_negotiation, punctuated_swap_reading, theater_ratio, 3, 0.4).

% Extraction over time
narrative_ontology:measurement(extract_preannounce, punctuated_swap_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(extract_shock_moment, punctuated_swap_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement(extract_stabilization, punctuated_swap_reading, base_extractiveness, 3, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(punctuated_swap_reading, global_infrastructure).
narrative_ontology:affects_constraint(punctuated_swap_reading, bretton_woods_inherent_instability).
narrative_ontology:affects_constraint(punctuated_swap_reading, triffin_inevitability_reading).
narrative_ontology:affects_constraint(punctuated_swap_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposing 'the fall of Bretton Woods.' The family includes punctuated_swap_reading (ε≈0.38, discrete choice framing), triffin_inevitability_reading (ε≈0.25, structural collapse framing), and overdetermined_composite_reading (ε≈0.42, multi-causal framing). Each story has its own epsilon value because the observables used to evaluate them are structurally distinct: (1) punctuated_swap measures institutional agency (did alternatives exist? what was decided?), (2) triffin measures mathematical necessity (could the system have persisted?), (3) overdetermined measures causal saturation (how many independent sufficient causes were present?). All three stories are true — they highlight different structural elements of the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(punctuated_swap_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
