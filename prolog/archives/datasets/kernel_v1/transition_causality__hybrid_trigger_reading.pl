% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__hybrid_trigger_reading, []).

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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Transition (Hybrid Trigger Reading): Structural Contradictions + Contingent Shocks
 *   domain: monetary_economics/international_finance/political_economy
 *
 * SUMMARY:
 *   The Bretton Woods system's transition to floating exchange rates
 *   represents a classic case of structural contradiction (Triffin Dilemma)
 *   whose actualization required contingent trigger events. The Triffin
 *   Dilemma posed an inherent mathematical tension: the US dollar
 *   simultaneously served as international reserve currency (requiring
 *   stability and unlimited supply) and as a reserve of value (requiring
 *   scarcity and discipline). By the late 1950s, this contradiction was
 *   theoretically recognized and structurally embedded. Yet the system
 *   persisted for another 12-15 years, during which specific historical
 *   shocks — Vietnam War fiscal expansion (1965+), French gold conversions
 *   (1965-1968), sterling crisis (1967), and escalating speculative runs —
 *   accumulated pressure until Nixon's August 1971 decision to close the gold
 *   window became inevitable. This reading instantiates the hybrid mechanism:
 *   structural contradictions create necessary (but not sufficient)
 *   conditions for collapse; contingent trigger events provide the sufficient
 *   conditions that activate collapse. The counterfactual is non-trivial: had
 *   Vietnam War spending been smaller, had French policy been less
 *   confrontational, had speculators been less coordinated, the system could
 *   have survived longer — possibly enabling institutional reforms (SDR
 *   expansion, gold revaluation, explicit devaluation) that might have
 *   extended Bretton Woods' life or created a managed successor system. The
 *   actual collapse was neither pure choice (contingent_choice_reading) nor
 *   pure inevitability (overdetermined_collapse_reading) but rather a hybrid:
 *   the system was doomed by structure but the manner and timing of doom
 *   required history.
 *
 * KEY AGENTS:
 *   - US Treasury: Primary beneficiary (institutional/arbitrage) — captures seigniorage privilege and monetary autonomy from dollar-based system; exits via unilateral devaluation (Nixon 1971)
 *   - Bretton Woods Creditors (France, Germany, UK): Secondary victims (organized/constrained) — trapped in dollar holdings with depreciation risk; France attempts to constrain US extraction via gold conversions; UK suffers sterling crisis
 *   - Fixed-Peg Peripheral Economies (Developing nations, Southeast Asia): Primary victims (powerless/trapped) — forced to hold dollars, absorb imported inflation, lose monetary autonomy; cannot exit peg without capital flight
 *   - Currency Speculators: Opportunistic beneficiaries (moderate/constrained) — coordinate crisis dynamics through carry-trade unwind and sterling/dollar runs; profit from peg collapse
 *   - IMF / Bretton Woods Authority: Institutional coordinator (institutional/arbitrage) — manages the system's enforcement but ultimately powerless to prevent structural breakdown; maintains performative theater as enforcement crumbles
 *   - Analytical Observer: Structural analyst (analytical/analytical) — sees Triffin contradiction as inevitable but recognizes that specific trigger events (Vietnam, gold conversions, runs) were necessary to actualize the collapse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.58).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.62).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Transition (Hybrid Trigger Reading): Structural Contradictions + Contingent Shocks").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/international_finance/political_economy").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '02b24b84-a9f5-44b8-8cbc-31779463e1c2').
narrative_ontology:cs_kernel_codification('02b24b84-a9f5-44b8-8cbc-31779463e1c2', formalized).
narrative_ontology:cs_authority_grounding('02b24b84-a9f5-44b8-8cbc-31779463e1c2', extraction).
narrative_ontology:cs_interpretation_layer_present('02b24b84-a9f5-44b8-8cbc-31779463e1c2').
narrative_ontology:cs_reading_relation('02b24b84-a9f5-44b8-8cbc-31779463e1c2', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('02b24b84-a9f5-44b8-8cbc-31779463e1c2', transition_causality__overdetermined_collapse_reading, influences).
narrative_ontology:cs_axiom('02b24b84-a9f5-44b8-8cbc-31779463e1c2', foundational, triffin_dilemma_necessitates_eventual_collapse).
narrative_ontology:cs_axiom_status(triffin_dilemma_necessitates_eventual_collapse, holdable).
narrative_ontology:cs_axiom_grounding('02b24b84-a9f5-44b8-8cbc-31779463e1c2', triffin_dilemma_necessitates_eventual_collapse, empirically_contingent).
narrative_ontology:cs_axiom('02b24b84-a9f5-44b8-8cbc-31779463e1c2', foundational, contingent_triggers_modulate_collapse_timing_and_form).
narrative_ontology:cs_axiom_status(contingent_triggers_modulate_collapse_timing_and_form, holdable).
narrative_ontology:cs_axiom_grounding('02b24b84-a9f5-44b8-8cbc-31779463e1c2', contingent_triggers_modulate_collapse_timing_and_form, empirically_contingent).
narrative_ontology:cs_reference_frame('02b24b84-a9f5-44b8-8cbc-31779463e1c2', bretton_woods_equilibrium_with_contained_contradictions).
narrative_ontology:cs_drift_state('02b24b84-a9f5-44b8-8cbc-31779463e1c2', id_1971_nixon_shock_realization, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('02b24b84-a9f5-44b8-8cbc-31779463e1c2', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_treasury_seigniorage_capture).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, floating_currency_speculators).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, fixed_peg_peripheral_economies).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, bretton_woods_discipline_creditors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FIXED-PEG PERIPHERAL (SNARE) — Trapped by dollar commitment; bears full extraction burden through dollar's overvaluation and loss of monetary autonomy. No exit option: attempting to break peg triggers capital flight and external intervention. The Triffin contradiction extracts from these agents via forced dollar holding and imported inflation.
constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BRETTON WOODS CREDITOR (TANGLED ROPE) — Organized but constrained: benefits from dollar-pegged stability (coordination function), but extracts through dollar depreciation risk and US monetary autonomy. France organized around gold conversions (de Gaulle's challenge) to constrain US extraction. The Triffin dilemma coordinates international trade while extracting seigniorage asymmetry; active enforcement via IMF conditionality required to maintain peg.
constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US TREASURY / IMF AUTHORITY (ROPE) — Experiences the system as coordination mechanism enabling postwar trade and development. Benefits from seigniorage and monetary flexibility. Arbitrage option: can exit via unilateral devaluation (Nixon's choice 1971). The constraint solves the collective action problem of currency stabilization; extraction runs toward this agent.
constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BRETTON WOODS RITUAL (PITON) — The formal architecture (IMF Articles, gold-window commitment, Bretton Woods Conference protocols) persists as performative theater long after functional contradictions surface. Theater increases as enforcement becomes spectacle: US defends peg through rhetoric while running deficits; foreign central banks perform deference to dollar while accumulating gold claims. Theater ratio rises as underlying enforcement crumbles.
constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CURRENCY SPECULATOR (TANGLED ROPE) — Benefits from the peg's existence (clear trading signal, carry-trade coordination) but also coordinates currency crises through crisis-driven exits. Constrained by policy risk but has exit paths via currency markets. The Triffin dilemma creates profit opportunities (betting against dollar, timing the break) that organize speculative coordination.
constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / OVERDETERMINED VIEW (MOUNTAIN) — From a civilizational perspective, the Triffin dilemma appears as an immutable logical contradiction: no currency can simultaneously be a stable store of value AND provide unlimited liquidity for global trade. The mathematics is inexorable — the system MUST eventually break. However, this reading naturalizes the contingency: the actual break required Vietnam War fiscal shock (1965+) AND French gold conversions (1965-1968) AND speculative runs on sterling (1967) AND political decision to abandon peg (1971). Different timing or intensity of triggers could have extended or accelerated the collapse.
constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transition_causality__hybrid_trigger_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, TR),
    TR >= 0.70.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Rising from 0.22 (1950) to 0.80 (1971). The hybrid reading models accumulating structural extraction (Triffin dilemma taxes fixed-peg economies through seigniorage and real exchange rate loss) plus accelerating extraction through crisis dynamics (speculative runs, capital flight, forced devaluation). The trajectory reflects both the slow burn of contradictions and the sharp acceleration triggered by shocks. By 1970-1971, extraction is near-maximal: fixed-peg economies have lost all autonomy, speculators are extracting via crisis coordination, and US seigniorage is collapsing as confidence erodes. Suppression (0.62): High and rising from 0.20 to 0.95. Early years: passive suppression through structural lock-in (no alternative to dollar peg). Middle years (1960-1968): active suppression via capital controls, IMF conditionality, policy coordination to defend the peg. Later years (1968-1971): suppression becomes increasingly coercive as the US imposes wage-price controls, closes gold window in stages, threatens trade retaliation against countries pursuing devaluation. Theater ratio (0.48): Moderate and variable. Early theater (1950-1960): Bretton Woods appears genuine coordination mechanism. Rising theater (1960-1968): defense mechanisms become visible — US speeches about "confidence in the dollar," gold-pool management tactics, pressure on countries to hold dollars. Declining theater (1968-1971): as crisis accelerates, the ritual becomes unsustainable — gold-pool collapse (March 1968) exposes the fiction of gold-window sustainability. The theater trajectory differs from a pure piton (where theater remains high); instead, theater is medium-high (0.42-0.52) during the crisis phase, reflecting active enforcement through performative policy theater mixed with genuine crisis dynamics. Claimed type (tangled_rope): The system coordinates international trade (genuine coordination function) while extracting through seigniorage asymmetry and monetary autonomy concentration (asymmetric extraction). Requires active enforcement (yes): IMF conditionality, capital controls, policy coordination meetings, gold-pool management. Has beneficiaries (yes): US Treasury, currency speculators. Has victims (yes): fixed-peg economies, creditor nations trapped in depreciating dollar holdings.
 *
 * PERSPECTIVAL GAP:
 *   The hybrid reading generates substantial perspectival divergence. US Treasury sees coordination (Rope) — the system enables postwar trade recovery and reflects legitimate monetary leadership. Creditor nations see mixed coordination-extraction (Tangled Rope) — benefits from dollar stability but constrained by seigniorage loss and depreciation risk; France organizes to increase leverage (gold conversions). Peripheral economies see pure extraction (Snare) — trapped in overvalued peg, losing monetary autonomy, absorbing imported inflation. Speculators see profit opportunity embedded in coordination (Tangled Rope) — the peg creates clear trading signals but also creates the conditions for crisis-driven extraction. The Bretton Woods authority sees ritual performance (Piton) — maintaining the fiction of convertibility as enforcement capacity erodes. The analytical observer sees structural contradiction plus contingent activation (hybrid reading's classification) — the Triffin math is inexorable but the specific shock sequence mattered. This perspectival diversity is diagnostically rich: it reveals that no single type accurately captures the system's evolution. The hybrid reading's strength is precisely that it explains the perspectival multiplicity: different agents experience the same structural constraint differently because they occupy different positions relative to the trigger events and their timing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derivation varies sharply by agent and timeline. Early period (1950-1960): US benefits from positive d (low d → low chi) from beneficiary status with arbitrage exit; peripheral economies suffer from high d (trapped victims). The constraint appears as coordination from above (Rope) and extraction from below (Snare) — perspectival gap is maximal. Middle period (1960-1968): triggers begin to accumulate. Vietnam shock increases d for victims as suppression intensifies (capital controls, policy coordination). Gold conversions shift d for creditor nations (constrained emergence from passivity). Speculators' d increases as crisis coordination becomes visible. Theater begins rising as performative defense mechanisms emerge. Late period (1968-1971): d approaches 1.0 for all agents except beneficiary (US Treasury). Crisis coordination increases chi for speculators; complete extraction becomes visible for trapped agents; ritual theater becomes unsustainable. The directionality dynamics are crucial: they explain why the perspectives diverge from Rope/Snare (early) through Tangled Rope (middle) to near-Snare dominance (late). The hybrid reading's contribution is modeling the time-dependence of d: the structural contradictions are present from 1950 onward, but d values are modulated by trigger intensity, making the system appear more stable early and more extractive late. Different trigger sequencing would produce different d trajectories.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading resolves mandatrophy by distinguishing structural level (Triffin dilemma is a real mathematical constraint) from historical level (the specific actualization required contingent triggers). At the structural level, the system was bound to fail: a currency cannot simultaneously be stable and infinitely supplied. At the historical level, the failure required specific shocks timed in specific ways. This disambiguation prevents the false either-or: the system is neither purely chosen (contingent_choice reading) nor purely inevitable (overdetermined reading), but rather structurally constrained yet historically contingent. The analytical observer's mountain classification correctly identifies a structural limit (the mathematics is inexorable) but misses the contingency: absent Vietnam War escalation or French policy confrontation or speculative coordination, the system might have persisted 5-10 more years, possibly enabling institutional repairs. The hybrid reading's tangled-rope classification captures this: genuine coordination function (solving postwar trade stabilization) combined with asymmetric extraction (seigniorage concentration) + active enforcement (IMF, capital controls) + contingent trigger dynamics (specific shock sequences). The mandatrophy dissolves when we recognize that the six perspective types model different analytical scales: structure (mountain), coordination (rope), hybrid coordination-plus-extraction (tangled_rope), pure extraction (snare), degraded ritual (piton), and strategic opportunism (integrated via directionality dynamics through the tangled_rope middle classification).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_necessity_threshold,
    'What magnitude or combination of trigger events constitutes a necessary condition for collapse vs. merely accelerating an inevitable process?',
    'Counterfactual analysis: modeling Bretton Woods continuation under alternative shock sequences (smaller Vietnam deficits, delayed gold conversions, stronger US credibility). Historical contingency studies examining path-dependent branching points.',
    'If triggers are necessary: hybrid reading confirmed (medium counterfactual viability). If triggers merely accelerate inevitability: reading collapses toward overdetermined_collapse. If triggers could have been indefinitely delayed: reading collapses toward contingent_choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_necessity_threshold, conceptual, 'Whether trigger events are necessary conditions or merely acceleration mechanisms for collapse').

omega_variable(
    structural_contradiction_definition,
    'Does the Triffin dilemma constitute a structural contradiction (logically unsolvable) or a coordination problem (solvable with institutional innovation)?',
    'Theoretical analysis of alternative reserve systems (SDR expansion, multi-currency baskets, gold standards variants) and their viability under 1960s constraints. Economic historians'' assessment of whether Bretton Woods II-type arrangements could have stabilized the system.',
    'If truly contradictory: supports structural inevitability; reading edges toward overdetermined. If coordination problem: supports hybrid reading; system could have survived with different trigger responses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_contradiction_definition, conceptual, 'Whether Triffin dilemma is a structural contradiction or solvable coordination problem').

omega_variable(
    policy_response_counterfactual,
    'Could US policymakers have avoided the specific trigger sequence that caused collapse (e.g., tighter fiscal discipline, earlier dollar devaluation, gold-window closure without inflation)?',
    'Reconstruction of decision nodes: Vietnam War fiscal authorization (1965), gold-conversion policy (1965-1968), currency defense tactics (1967-1971). Analysis of political and institutional constraints on alternative choices at each decision point.',
    'If yes: supports contingent_choice or hybrid reading depending on magnitude of alternatives. If no: supports overdetermined reading; triggers were politically inevitable given structural positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_response_counterfactual, empirical, 'Whether US policy response to Triffin dilemma was contingent or politically determined').

omega_variable(
    sibling_foreclosure_tradeoff,
    'Does the hybrid reading''s emphasis on trigger contingency logically exclude the overdetermined reading''s emphasis on structural inevitability, or do both mechanisms operate at different analytical scales?',
    'Clarification of mechanism levels: overdetermined reading operates at structural-logic level (Triffin math is inexorable); hybrid reading operates at historical-contingency level (the specific way it collapsed required specific shocks). Can both be true simultaneously — structure determines THAT collapse must occur, contingency determines WHEN and HOW.',
    'If mechanisms are orthogonal: all three readings coexist as valid frameworks. If mutually exclusive: hybrid reading forecloses overdetermined at some analytical level. Clarity on this distinction determines whether reading_relations should be coexists_with or influences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_foreclosure_tradeoff, conceptual, 'Whether structural inevitability and historical contingency are orthogonal or mutually exclusive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 1950, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcht_theater_1950_postwar, transition_causality__hybrid_trigger_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(tcht_theater_1960_deception, transition_causality__hybrid_trigger_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(tcht_theater_1965_vietnam, transition_causality__hybrid_trigger_reading, theater_ratio, 1965, 0.42).
narrative_ontology:measurement(tcht_theater_1968_gold_window_ritual, transition_causality__hybrid_trigger_reading, theater_ratio, 1968, 0.52).
narrative_ontology:measurement(tcht_theater_1970_crisis, transition_causality__hybrid_trigger_reading, theater_ratio, 1970, 0.48).

% Extraction over time
narrative_ontology:measurement(tcht_extract_1950_postwar, transition_causality__hybrid_trigger_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(tcht_extract_1960_triffin_recognized, transition_causality__hybrid_trigger_reading, base_extractiveness, 1960, 0.35).
narrative_ontology:measurement(tcht_extract_1965_vietnam_shock, transition_causality__hybrid_trigger_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(tcht_extract_1968_gold_pool_collapse, transition_causality__hybrid_trigger_reading, base_extractiveness, 1968, 0.62).
narrative_ontology:measurement(tcht_extract_1970_sterling_crisis, transition_causality__hybrid_trigger_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(tcht_extract_1971_nixon_shock, transition_causality__hybrid_trigger_reading, base_extractiveness, 1971, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(tcht_supp_1950_postwar, transition_causality__hybrid_trigger_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(tcht_supp_1960_capital_controls, transition_causality__hybrid_trigger_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(tcht_supp_1965_vietnam, transition_causality__hybrid_trigger_reading, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement(tcht_supp_1968_gold_pool, transition_causality__hybrid_trigger_reading, suppression_requirement, 1968, 0.72).
narrative_ontology:measurement(tcht_supp_1971_final, transition_causality__hybrid_trigger_reading, suppression_requirement, 1971, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__hybrid_trigger_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, triffin_dilemma_structural_contradiction).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, vietnam_war_fiscal_shock).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, french_gold_conversion_strategy).

% DUAL FORMULATION NOTE:
% The transition_causality kernel decomposes into three constraint stories (three readings of the same contested origin): contingent_choice_reading, hybrid_trigger_reading (this story), and overdetermined_collapse_reading. Each reading models a different causal mechanism operating at a different analytical scale. This story (hybrid_trigger_reading) links to the sibling readings and to upstream structural constraints (Triffin dilemma) and trigger events (Vietnam War, French policy, speculative dynamics). Each reading gets its own ε, its own perspectives, and its own classification type. Authoring discipline: this reading does NOT describe the alternative readings or average ε across them — it instantiates the hybrid mechanism cleanly (structural contradictions + contingent triggers) with its own stable ε=0.58.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, organized, 0.52).
constraint_indexing:directionality_override(transition_causality__hybrid_trigger_reading, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
