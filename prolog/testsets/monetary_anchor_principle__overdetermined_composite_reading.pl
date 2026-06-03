% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__overdetermined_composite_reading, []).

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
 *   constraint_id: monetary_anchor_principle__overdetermined_composite_reading
 *   human_readable: Gold Standard Collapse via Overdetermined Composite Pressures (1960s-1971)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The collapse of the Bretton Woods gold standard (August 15, 1971)
 *   represents a constraint that emerged from the overdetermined convergence
 *   of four structural pressures: (1) the Triffin dilemma—the mathematical
 *   impossibility of a single nation simultaneously maintaining gold-standard
 *   parity, defending against runs, and providing global liquidity; (2)
 *   Vietnam War deficits—sustained fiscal expansion that undermined reserve
 *   accumulation and confidence in gold backing; (3) technological capital
 *   mobility—the rapid integration of offshore money markets (Eurodollars)
 *   that enabled capital flight outside regulatory perimeters; (4) Keynesian
 *   policy consensus—the intellectual shift away from commodity-standard
 *   discipline toward demand-management primacy. This reading instantiates
 *   the 'overdetermined composite' interpretation of the monetary anchor
 *   principle: none of these four pressures alone would have forced collapse
 *   by 1971, but the convergent operation of all four made the system
 *   unsustainable by the late 1960s. The constraint is tangled_rope because
 *   it combines genuine coordination function (Bretton Woods did stabilize
 *   postwar exchange rates and enable multilateral trade recovery) with
 *   asymmetric extraction (the US captured macroeconomic autonomy while
 *   externalizing discipline costs to trading partners and the system
 *   itself). The extraction mechanism is not reversible without
 *   simultaneously addressing all four causal streams—a feature that
 *   distinguishes this reading from the single-cause 'Triffin inevitability'
 *   and discrete-choice 'punctuated swap' framings.
 *
 * KEY AGENTS:
 *   - US Fiscal Authority / Keynesian Policy Coalition: Primary beneficiary (institutional/arbitrage) — captured macroeconomic policy autonomy for Vietnam War and Great Society spending; externalized inflation discipline to fixed-rate partners
 *   - Global Monetary Discipline Regime (fixed exchange rates, commodity standard): Primary victim (powerless/trapped) — bears full cost of system maintenance; cannot escape overdetermined pressures through single-stream relief
 *   - Non-Reserve Currency Central Banks (Germany, France, others): Secondary victim (powerful/constrained) — subordinated to US hegemony; forced to absorb US inflation and accumulate dollars as de facto reserve assets; could not unilaterally abandon system
 *   - Private Capital Markets / Eurodollar Operators: Organized beneficiary-victims (powerful/constrained to arbitrage) — exploit interest-rate differentials and devaluation expectations; simultaneously trapped by the system's inability to provide genuine capital mobility
 *   - Bretton Woods Institutional Apparatus (IMF, BIS, central bank coordination): Organized enforcer (organized/constrained) — genuinely coordinates exchange-rate stability and liquidity provision while increasingly becoming a mechanism for deferring collapse
 *   - Gold Market & Physical Commodity Mechanism: Structural victim (implicit, non-agent) — gold scarcity and extraction costs make large-scale reserve accumulation economically irrational; the market mechanism itself signals system unsustainability
 *   - Analytical Observer: Sees all four streams as converging inevitabilities (analytical/analytical) — risks naturalizing policy choices (Keynesianism, fiscal deficits, capital mobility policy) as natural laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, 0.62).
domain_priors:suppression_score(monetary_anchor_principle__overdetermined_composite_reading, 0.48).
domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(monetary_anchor_principle__overdetermined_composite_reading, "Gold Standard Collapse via Overdetermined Composite Pressures (1960s-1971)").
narrative_ontology:topic_domain(monetary_anchor_principle__overdetermined_composite_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(monetary_anchor_principle__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__overdetermined_composite_reading, 'f6c8b97b-c267-4328-83a5-e67b8dae43f4').
narrative_ontology:cs_kernel_codification('f6c8b97b-c267-4328-83a5-e67b8dae43f4', formalized).
narrative_ontology:cs_authority_grounding('f6c8b97b-c267-4328-83a5-e67b8dae43f4', extraction).
narrative_ontology:cs_interpretation_layer_present('f6c8b97b-c267-4328-83a5-e67b8dae43f4').
narrative_ontology:cs_reading_relation('f6c8b97b-c267-4328-83a5-e67b8dae43f4', monetary_anchor_principle__triffin_inevitability_reading, influences).
narrative_ontology:cs_reading_relation('f6c8b97b-c267-4328-83a5-e67b8dae43f4', monetary_anchor_principle__punctuated_swap_reading, coexists_with).
narrative_ontology:cs_axiom('f6c8b97b-c267-4328-83a5-e67b8dae43f4', foundational, multistream_structural_convergence).
narrative_ontology:cs_axiom_status(multistream_structural_convergence, holdable).
narrative_ontology:cs_axiom_grounding('f6c8b97b-c267-4328-83a5-e67b8dae43f4', multistream_structural_convergence, empirically_contingent).
narrative_ontology:cs_axiom('f6c8b97b-c267-4328-83a5-e67b8dae43f4', secondary, extraction_symmetry_across_streams).
narrative_ontology:cs_axiom_status(extraction_symmetry_across_streams, holdable).
narrative_ontology:cs_axiom_grounding('f6c8b97b-c267-4328-83a5-e67b8dae43f4', extraction_symmetry_across_streams, deontological).
narrative_ontology:cs_reference_frame('f6c8b97b-c267-4328-83a5-e67b8dae43f4', commodity_standard_monetary_discipline).
narrative_ontology:cs_drift_state('f6c8b97b-c267-4328-83a5-e67b8dae43f4', late_1960s_accumulation_phase, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f6c8b97b-c267-4328-83a5-e67b8dae43f4', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__overdetermined_composite_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_authority).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__overdetermined_composite_reading, keynesian_policy_coalition).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, global_monetary_discipline).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, fixed_exchange_rate_system).
narrative_ontology:constraint_victim(monetary_anchor_principle__overdetermined_composite_reading, commodity_standard_credibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL MONETARY DISCIPLINE (SNARE) — The fixed exchange rate system and commodity-standard credibility are trapped by overdetermined pressures they cannot escape. Capital mobility, fiscal deficits, and structural gold scarcity converge to exhaust alternatives. Perceived extractiveness is maximal: the discipline regime bears all costs of regime maintenance while beneficiaries capture asymmetric gains from escape.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-RESERVE CURRENCY CENTRAL BANKS (TANGLED ROPE) — Constrained by gold standard system; benefit from Bretton Woods coordination and exchange-rate stability, but also bear subordination to US monetary hegemony and forced absorption of US inflation. As gold supplies tighten and US deficits accumulate, these actors face compounding costs with no unilateral exit.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: US FISCAL AUTHORITY / KEYNESIAN CONSENSUS (ROPE) — Primary beneficiary. Experiences the constraint as enabling: gold standard allows deficit spending to fund Vietnam War and domestic Great Society without immediate inflation feedback. The institution captures macroeconomic policy autonomy while externalizing discipline costs to fixed-rate trading partners and gold reserves. Classified as Rope from the beneficiary's immediate horizon because the coordination function (global liquidity provision) is genuine, even as extraction runs asymmetrically.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVATE CAPITAL MARKETS / ARBITRAGE TRADERS (TANGLED ROPE) — Face compounding constraints: capital mobility increases but the fixed-rate regime prevents full arbitrage; interest rate differentials and expected devaluation create profit opportunities that reinforce the extraction mechanism. Generational view reveals: as speculation accumulates, the beneficiaries from arbitrage extract value from the maintenance costs of the fixed system, but face binary endgame (devaluation or capital controls) with no middle ground.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: BRETTON WOODS INSTITUTIONAL APPARATUS (TANGLED ROPE) — Organized actors managing the system (IMF, BIS, central bank coordination) face genuine coordination function (maintaining exchange-rate stability, providing liquidity facilities) alongside extraction: as overdetermined pressures accumulate, the apparatus becomes a mechanism for deferring collapse rather than preventing it. By late 1960s, institutional maintenance IS the constraint — agents are enforcing a known-to-be-unsustainable system because no agreed exit exists.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: GOLD RESERVE COVERAGE RITUAL (PITON) — The formal requirement for gold backing has atrophied from substantive constraint to performance: by late 1960s, the US gold reserve is inadequate to cover money stock and outstanding claims, yet the formal ratio continues to be cited as if it constrains policy. Theater_ratio high (0.35 of system maintenance is performative coverage ratios and regulatory mythology) because the ritual persists through institutional inertia despite structural obsolescence.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational/universal perspective treating the four causal streams as independent constraints (Triffin dilemma as mathematical necessity, capital mobility as technological law, fiscal deficits as policy outcome, Keynesian consensus as epistemic shift), the collapse appears inevitable — an overdetermined system with no escape. However, this perspective risks false-summit classification: the analytically perceived 'inevitability' naturalizes what are actually contingent institutional choices (the Keynesian consensus was not inevitable, capital mobility was policy-enabled, the deficit was a choice). The engine flags this as naturalization of policy.
constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monetary_anchor_principle__overdetermined_composite_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monetary_anchor_principle__overdetermined_composite_reading, TR),
    TR >= 0.70.

:- end_tests(monetary_anchor_principle__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High, reflecting the structural asymmetry between beneficiaries capturing macroeconomic autonomy and victims bearing system-maintenance costs. The measurement trajectory (0.35 → 0.62, 1960-1971) tracks the accumulation of overdetermination: early 1960s, the system appeared sustainable if discipline were restored; by 1968, reserve exhaustion signals closed escape routes; by 1971, all four causal streams are in force. Suppression (0.48): Moderate-high. The system required active enforcement through capital controls (interest equalization tax, voluntary foreign credit restraint), the London Gold Pool coordination, and Bretton Woods institutional surveillance. As pressures accumulated, suppression requirements intensified (0.25 → 0.58) because the system had to work harder to prevent capital flight and reserve depletion. Theater ratio (0.35): Moderate. The gold reserve coverage ratio became increasingly performative — by late 1960s, the formal ratio was known to be inadequate, yet continued to be cited as if it constrained policy (the ritual persisted). Claimed type (tangled_rope): Justified by the combination of genuine coordination function (exchange-rate stability, multilateral liquidity provision) with asymmetric extraction (US policy autonomy, inflation export, discipline bypass). The tangled_rope gate requires beneficiaries + victims + active enforcement, all present.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power and exit levels. The US fiscal authority (institutional/arbitrage) sees coordination—the system enabling postwar growth and policy flexibility—while non-reserve central banks (powerful/constrained) see extraction—subordination to US hegemony and forced dollar accumulation. Private capital markets (organized, but with growing arbitrage opportunities) experience the system as increasingly contradictory: arbitrage opportunities multiply as confidence erodes, yet the fixed-rate regime prevents full capital mobility. The Bretton Woods apparatus (organized enforcer) transitions from believing the system sustainable to managing its decline. The discipline regime itself (implicit victim) has no perspective—it is the constraint being experienced. The piton perspective on gold reserve coverage captures the ritualization of the system: formal compliance continues through institutional inertia even as structural obsolescence is apparent. The analytical observer risks false-summit classification by treating the four causal streams as inevitable natural laws rather than contingent policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is derived from beneficiary/victim status and exit-option constraints. The US fiscal authority (beneficiary with arbitrage options: capital inflows, policy autonomy, US-dollar hegemony) derives low d (~0.15), producing negative χ — experienced extraction runs toward them. Non-reserve central banks (victims constrained by gold obligation and dollar subordination) derive high d (~0.75), producing high χ — maximum experienced extraction. Private capital markets (organized with growing arbitrage capacity but blocked by fixed rates) derive moderate-high d (~0.60), reflecting the constraint's asymmetry: arbitrage opportunities exist but are suppressed by the fixed-rate regime itself. The Bretton Woods apparatus (organized enforcers) derives moderate d (~0.50) — they have policy agency but are increasingly captured by the contradiction between coordination function and extraction mechanism. The discipline regime itself would have d approaching 1.0 if it could be an agent — it bears all costs and has no exit. These directionality profiles confirm tangled_rope: the beneficiary experiences rope-like coordination (low χ, legitimate first-mover benefit), the victims experience snare-like extraction (high χ, trapped alternatives), and the intermediate actors experience tangled dynamics (genuine coordination alongside suppression of exit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating how tangled_rope requires simultaneous coordination and extraction, neither reducible to the other. The coordination function (exchange-rate stability, liquidity provision) was genuine — Bretton Woods succeeded in avoiding the competitive devaluations and trade wars of the 1930s. The extraction function (US asymmetric policy autonomy, inflation export, discipline bypass) was equally genuine. The system was not simply 'coordination with some exploitation' but a structural fusion where the coordination mechanism itself enabled the extraction and vice versa: liquidity provision required dollar accumulation, which required US deficits, which enabled US policy autonomy, which drove the inflation that forced the contradictions. No single measure (fixing gold price, expanding SDRs, imposing capital controls) could restore equilibrium because equilibrium would require abandoning either the coordination or the autonomy. The mandatrophy resolves when the reader understands that 'eventually the system had to collapse' is not the same as 'the collapse was inevitable from the start'—the system's internal contradictions were real but not deterministic. Collapse occurred when the balance of leverage shifted and coordination benefits no longer outweighed extraction costs for enough actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_policy_reversibility,
    'Could the gold standard have been sustained through 1971 if any single causal stream (Triffin pressure, Vietnam War deficit, capital controls, or Keynesian policy consensus) had been interrupted?',
    'Counterfactual structural analysis: model the system under scenarios where each causal stream is independently severed (e.g., no Vietnam War spending, aggressive capital controls, orthodox monetary policy) and assess whether gold-standard sustainability emerges.',
    'If sustainable under any single-stream interruption: epsilon should be lower (say 0.45), and the constraint is a hybrid Tangled Rope with some realistic alternatives (suggests Scaffold elements). If collapse is inevitable even with interruption: epsilon stays high (0.62+), confirming true overdetermination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_policy_reversibility, conceptual, 'Whether interrupting any single causal stream would have preserved the gold standard').

omega_variable(
    reading_decomposition_boundary,
    'Is this ''overdetermined composite'' reading meaningfully distinct from the ''Triffin inevitability'' reading, or does the addition of Vietnam War, capital mobility, and Keynesian consensus merely multiply specifications of a single underlying structural cause (the Triffin dilemma)?',
    'Structural analysis of axiom independence: if the Triffin dilemma alone constrains gold-standard sustainability to an identical 1968-1973 window, the competing readings collapse into one; if the other three factors independently shift the collapse window or change the nature of the constraint, they are truly separable causal streams.',
    'If readings collapse: this constraint should be merged with Triffin_inevitability_reading and cs_structure.reading_relations should be updated. If truly separable: omega resolves in favor of the current decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_decomposition_boundary, conceptual, 'Whether overdetermined-composite and Triffin-inevitability readings are genuinely distinct').

omega_variable(
    temporal_onset_of_inevitability,
    'At what date did the system become truly overdetermined—i.e., when did the last single-stream escape route close? Was it Triffin''s 1960 publication (cognitive shift), the 1965 escalation in Vietnam (fiscal shock), the 1968 London Gold Pool collapse (reserve exhaustion signal), or the late-1960s acceleration of capital mobility?',
    'Historical reconstruction of policy options and deliberation records; identification of the moment when officials ceased treating the system as sustainable and began planning transition mechanisms.',
    'If onset is 1960 (Triffin recognized): the reading is about epistemic inevitability (known but not acted upon). If onset is 1965-1968 (Vietnam + reserves): the reading is about structural exhaustion. If onset is late 1960s (capital mobility): the reading emphasizes financial innovation as final pressure. Different onset dates shift the narrative framing and may support different cs_structure.drift_state characterizations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_onset_of_inevitability, empirical, 'When the system became structurally overdetermined (all escape routes closed)').

omega_variable(
    beneficiary_agency_vs_structural_compulsion,
    'Did the US fiscal authority actively choose deficit spending knowing it would force gold-standard abandonment (agency), or were deficits (Vietnam War, Great Society) driven by factors independent of monetary collapse (structural compulsion)?',
    'Document analysis of policy deliberation 1965-1971; assess whether monetary-system considerations were present in Vietnam War budget decisions and domestic spending debates.',
    'If high agency: the beneficiary (US fiscal authority) extracted value through deliberate policy; classify as snare-like extraction by a powerful actor. If compulsion: the system collapsed despite beneficiary intentions; the tangled_rope classification better reflects mixed causality. Affects directionality interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_agency_vs_structural_compulsion, empirical, 'Whether US deficit spending was deliberate (knowing consequence of monetary collapse) or independent of it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__overdetermined_composite_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monanchor_theater_1960, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(monanchor_theater_1968, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(monanchor_theater_1971, monetary_anchor_principle__overdetermined_composite_reading, theater_ratio, 8, 0.35).

% Extraction over time
narrative_ontology:measurement(monanchor_extrac_1960, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(monanchor_extrac_1965, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(monanchor_extrac_1968, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(monanchor_extrac_1971, monetary_anchor_principle__overdetermined_composite_reading, base_extractiveness, 8, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(monanchor_suppress_1960, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(monanchor_suppress_1965, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(monanchor_suppress_1968, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(monanchor_suppress_1971, monetary_anchor_principle__overdetermined_composite_reading, suppression_requirement, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__overdetermined_composite_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__overdetermined_composite_reading, 0.18).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, triffin_dilemma_mathematical_limit).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, us_fiscal_deficits_1965_1971).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, eurodollar_market_capital_mobility).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, keynesian_consensus_discipline_shift).
narrative_ontology:affects_constraint(monetary_anchor_principle__overdetermined_composite_reading, gold_standard_sustainability).

% DUAL FORMULATION NOTE:
% The overdetermined-composite reading treats the Triffin dilemma, Vietnam War deficits, capital mobility, and Keynesian consensus as four structurally distinct constraints that interact to force gold-standard collapse. Each upstream constraint has its own epsilon (Triffin dilemma as mathematical limit, capital mobility as enabling infrastructure, fiscal deficits as policy outcome, Keynesian consensus as epistemic regime). The composite reading captures the interdependence: none alone forces collapse, but convergence does. Sibling readings decompose the kernel differently (Triffin-inevitability privileges the mathematical limit; punctuated-swap privileges the institutional decision). See 'monetary_anchor_principle__triffin_inevitability_reading' and 'monetary_anchor_principle__punctuated_swap_reading' for alternative decompositions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monetary_anchor_principle__overdetermined_composite_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
