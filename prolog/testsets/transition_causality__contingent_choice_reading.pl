% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_transition_causality__contingent_choice_reading, []).

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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: The Bretton Woods Transition as Contingent Policy Choice
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods monetary system (1944-1971) was a hybrid
 *   coordination-extraction regime that coordinated international trade and
 *   capital flows while simultaneously concentrating monetary autonomy in the
 *   U.S. Federal Reserve. Under this reading (contingent choice), the 1971
 *   transition to floating exchange rates was a deliberate policy decision by
 *   the Nixon administration, not a structural inevitability. The regime had
 *   genuine coordination functions: fixed exchange rates enabled trade
 *   planning and capital stability for decades. However, the constraint
 *   increasingly extracted through monetary subordination (the Triffin
 *   dilemma), seigniorage transfer, and constraints on U.S. domestic monetary
 *   policy. By 1968-1971, multiple policy alternatives existed for reform
 *   (Triffin proposals, reserve currency basketing, SDR elevation), yet the
 *   U.S. chose unilateral exit via the August 1971 Nixon Shock. This reading
 *   emphasizes policy agency and contingency: different choices at the
 *   decision node would have produced different outcomes. The constraint's
 *   death was thus contingent on a deliberate institutional decision, not
 *   structurally overdetermined.
 *
 * KEY AGENTS:
 *   - U.S. Monetary Authority (Federal Reserve/Treasury): Beneficiary (institutional/arbitrage) — gained monetary autonomy and seigniorage. The policy choice reflected this institutional interest.
 *   - Pegged Currency Nations: Victim (powerless/trapped) — bore costs of inflationary transfer and unilateral regime change. No voice in the decision that restructured their constraint.
 *   - Bretton Woods Beneficiary Nations (Western Europe, Japan): Mixed (organized/constrained) — benefited from fixed rates but also subordinated to U.S. policy cycles.
 *   - International Institutions (IMF/IBRD): Institutional actor (institutional/constrained) — dependent on U.S. backing, unable to redesign regime rules without U.S. consent.
 *   - Global Financial Capital: Powerful actor (powerful/mobile) — experienced Bretton Woods as a temporary coordination framework with a clear exit path as capital mobility constraints increased.
 *   - Gold Standard Doctrine: Institutional vestige (institutional/arbitrage) — maintained through inertia despite atrophied intellectual legitimacy.
 *   - Analytical Observer: Positions this reading's causal claim at the decision node rather than structural inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.38).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.42).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "The Bretton Woods Transition as Contingent Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, 'dfed89ff-c08d-42bb-ba91-d271a20eb6b9').
narrative_ontology:cs_kernel_codification('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', fixed_text).
narrative_ontology:cs_authority_grounding('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', extraction).
narrative_ontology:cs_interpretation_layer_present('dfed89ff-c08d-42bb-ba91-d271a20eb6b9').
narrative_ontology:cs_reading_relation('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', foundational, policy_alternatives_were_viable).
narrative_ontology:cs_axiom_status(policy_alternatives_were_viable, holdable).
narrative_ontology:cs_axiom_grounding('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', policy_alternatives_were_viable, empirically_contingent).
narrative_ontology:cs_axiom('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', foundational, nixon_decision_reflected_institutional_agency).
narrative_ontology:cs_axiom_status(nixon_decision_reflected_institutional_agency, holdable).
narrative_ontology:cs_axiom_grounding('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', nixon_decision_reflected_institutional_agency, empirically_contingent).
narrative_ontology:cs_reference_frame('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', bretton_woods_gold_standard_legitimacy).
narrative_ontology:cs_drift_state('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', post_1968_london_gold_pool_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dfed89ff-c08d-42bb-ba91-d271a20eb6b9', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, u_s_monetary_autonomy).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, u_s_capital_markets).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_exchange_rate_beneficiaries).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, pegged_currency_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEGGED CURRENCY NATIONS (SNARE) — Trapped by dollar-peg dependencies; no viable exit without redenomination crisis. Bears extraction through monetary subordination and inflationary transfer. The U.S. policy choice (contingent in this reading) forced exit from fixed-rate regime without alternatives. Maximum experienced extraction — no agency, no voice in the decision that restructured their constraints.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: BRETTON WOODS BENEFICIARY NATIONS (TANGLED ROPE) — Countries that benefited from fixed rates (trade stability, capital planning) also faced extraction through dollar hegemony and seigniorage transfer. Benefited from coordination (predictable exchange rates) while bearing cost of subordination to U.S. monetary cycles. Constrained exit: could not maintain pegs without matching U.S. policy, yet the policy choice that dissolved the constraint was unilateral and unexpected.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: U.S. MONETARY AUTHORITY (ROPE) — Experiences the Bretton Woods constraint as a coordination mechanism: the dollar peg enabled predictable international trade and U.S. capital market access. But the constraint also limited domestic monetary policy autonomy. From the institutional perspective, the constraint was a genuine coordination solution with manageable extraction overhead. The policy choice (contingent) was to resolve this by exiting the regime rather than reforming it.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BRETTON WOODS INSTITUTIONS (IMF/IBRD) (TANGLED ROPE) — The institutions coordinated international finance under the peg regime (genuine coordination function) but also enforced compliance and adjustment pressures (extraction mechanism). The U.S. policy choice unilaterally restructured the rule-set these institutions were built to administer. Constrained: dependent on U.S. backing, unable to redesign without U.S. consent, yet the policy choice was U.S. unilateral.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL FINANCIAL CAPITAL (SCAFFOLD) — The Bretton Woods peg constrained capital mobility and interest rate arbitrage. Capital sees the regime as temporary coordination with a clear sunset path. Mobile agents with resources to relocate across regimes experience the constraint as a policy-solvable problem with an exit available to organized, resource-rich actors. The policy choice (contingent from this perspective) unblocked capital flows — treating the constraint as a sunset mechanism.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: GOLD STANDARD DOCTRINE (PITON) — The gold standard's intellectual legitimacy had been atrophying for decades (Keynes critiqued it in the 1920s; Bretton Woods was already a diluted form). By 1971, the doctrine was maintained through institutional inertia rather than functional necessity. Policymakers had alternatives (Triffin proposals, SDR elevation, multicurrency reserve system) but defaulted to the inherited doctrine. The gold peg persisted as theater — a legitimacy performance that had lost economic function.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - CONTINGENT CHOICE (TANGLED ROPE) — The Bretton Woods constraint functioned as genuine international coordination (stabilized exchange rates, enabled trade, created institutional architecture) overlaid with asymmetric extraction (U.S. seigniorage, monetary subordination, policy constraints on U.S. autonomy). The transition was contingent on Nixon's 1971 decision — a deliberate policy choice among alternatives, not an inevitable collapse. The reading sees the constraint as structurally changeable through different policy choices; the actual transition reflected institutional power asymmetries rather than structural necessity.
constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(transition_causality__contingent_choice_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(transition_causality__contingent_choice_reading, TR),
    TR >= 0.70.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Bretton Woods regime functioned as genuine international coordination (trade stability, capital flows, predictable rates) with embedded extraction mechanisms (U.S. seigniorage, monetary subordination, constraint on U.S. policy autonomy). The 0.38 value reflects a hybrid: real coordination benefits for many actors, real extraction costs for others, with the asymmetry concentrated rather than universal. Measurement trajectory shows rising extractiveness from 1950 (0.25) to 1971 (0.42), reflecting the Triffin dilemma's accumulation — as U.S. external deficits grew, the extractive mechanics became more salient. Suppression (0.42): Moderate. Nations were constrained by the fixed-rate regime but not trapped — alternatives existed (reserve redenomination, bilateral agreements, capital controls). U.S. policy choice was also constrained by gold reserve requirements and international legitimacy dependence. The suppression requirement rose over time (1950: 0.15 → 1971: 0.42) as the regime's contradictions required increasing enforcement effort (Bretton Woods II adjustments, gold pool interventions, capital controls). Theater ratio (0.35): Low-moderate. The regime was functionally real — exchange rate stability, capital controls, and monetary coordination operated as genuine mechanisms, not purely performative. However, the gold standard component was increasingly theatrical by 1968-1971: the gold price peg was maintained through market interventions despite market pressure, and policymakers knew the official gold price was unsustainable. By 1971, defending the gold peg became almost entirely performative — the London Gold Pool had collapsed in 1968, the two-tier system was a fiction, and Nixon's decision was essentially formalizing what had already occurred informally.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this reading centers on whether the regime's death was chosen or forced. The U.S. authority sees Rope (coordination mechanism with manageable constraints on autonomy; exit is a policy choice). Pegged nations see Snare (subordination without exit). Bretton Woods beneficiary nations see Tangled Rope (both benefits and extraction). The analytical observer (this reading's analytical perspective) sees Tangled Rope with contingent termination: the constraint was structurally changeable through different policy choices. By contrast, the overdetermined reading would shift the analytical classification toward Mountain or toward universally experienced Snare — the reading would emphasize that structural contradictions made the regime's death inevitable regardless of policy choice. The contingent choice reading maintains the tangled rope classification because it treats policy agency as a real structural variable: U.S. institutions genuinely could have chosen differently, and different choices would have produced different terminal regimes.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading derives directionality from the structural position of each agent relative to the regime's coordination-extraction balance. The U.S. is the primary beneficiary with full arbitrage: it could exit (and did, via policy choice) or redesign rules (and negotiated floating rates post-transition). Beneficiary status + arbitrage exit → low d → negative effective extraction (the U.S. experiences the regime as coordination opportunity, with extraction overhead flowing outward). Pegged nations are trapped primary victims: they cannot exit the dollar-peg regime without redenomination crisis, and the U.S. policy choice imposed exit costs without their consent. Victim status + trapped exit → high d → high chi, producing the snare classification. Bretton Woods beneficiary nations occupy the middle: genuinely benefited from fixed rates (d ~0.35) but also bore subordination costs and faced constrained exit (redenominate or float unilaterally, both costly). The contingent choice reading treats policy agency as the causal lever: different choices at the decision node would have altered the beneficiary/victim structure and the regime's terminal state. The reading thus locates extractiveness primarily in the distribution of agency to choose exit conditions, not in structural inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate conflict (tangled rope) is resolved by attending to the reading's specific causal framing. The regime genuinely coordinated (fixed rates enabled trade and capital planning) and genuinely extracted (U.S. seigniorage, monetary subordination). The mandate is NOT to adjudicate whether coordination or extraction was primary — the answer is 'both.' Rather, the mandate is to show that the constraint's terminal state (floating rates) was chosen via institutional agency, not predetermined by structural mechanics. This reading meets the mandate by showing: (1) Genuine coordination function (justifies rope classification from U.S./beneficiary perspective), (2) Genuine asymmetric extraction (justifies snare from victim perspective), (3) Agency to choose terminal state differently (justifies tangled rope from analytical perspective, not mountain). The alternative readings would argue the terminal state was overdetermined or hybrid-triggered, but this reading asserts policy contingency throughout.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_policy_viability,
    'Were alternative policy responses to the 1970s monetary crises structurally viable within the Bretton Woods framework, or had the system''s internal contradictions foreclosed all options?',
    'Historical counterfactual analysis: modeling of Triffin proposals, French reserve system alternatives, SDR elevation scenarios, and multicurrency baskets. Comparison with actual policy constraints cited by contemporaneous policymakers vs. structural constraints.',
    'If alternatives were viable: reading supports contingent choice frame (Nixon''s decision was one option among several). If alternatives were foreclosed: reading weakens toward hybrid or overdetermined frame. Magnitude of viable alternatives determines claim strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_policy_viability, empirical, 'Whether policy alternatives to the 1971 decision were structurally feasible').

omega_variable(
    nixon_agency_vs_structural_pressure,
    'How much of Nixon''s August 1971 decision reflected deliberate policy choice (agency) vs. reactive response to exhausted gold reserves and political pressure?',
    'Documentary analysis of Nixon''s decision records, Kissinger cables, Burns-Nixon conversations, and White House tapes. Quantification of timing: did policy choice precede constraint activation (agency) or follow it (reaction)?',
    'If decision preceded constraint activation: strong agency signal, contingent choice frame confirmed. If decision followed exhaustion of alternatives: contingency is significantly constrained, hybrid frame gains credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nixon_agency_vs_structural_pressure, empirical, 'Whether Nixon''s 1971 decision reflected choice or reactive necessity').

omega_variable(
    bretton_woods_coordination_surplus,
    'Did Bretton Woods distribute genuine coordination benefits, or was the regime primarily an extraction mechanism disguised as coordination?',
    'Comparative analysis of trade stability, capital allocation efficiency, and growth correlates under fixed-rate regime vs. successor floating regimes. Measurement of which nations gained from fixed rates and which bore costs; temporal analysis of coordination surplus extraction.',
    'If genuine coordination surplus existed: the regime''s death was loss (contingent choice reading emphasizes this). If primarily extraction mechanism: the regime''s demise was relief (supports overdetermined reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bretton_woods_coordination_surplus, empirical, 'Whether Bretton Woods produced genuine coordination benefits or primarily enabled extraction').

omega_variable(
    reading_identity_ambiguity,
    'Does this reading instantiate ''contingent choice'' (policy decision among alternatives) or ''contingent triggering'' (structural contradictions requiring a trigger event)?',
    'Semantic/axiological distinction: the contingent choice reading claims Nixon could have chosen differently at the decision node; the hybrid reading claims structural contradictions required some trigger but didn''t determine which trigger. The distinction locates agency: in the choice (contingent) vs. in the constraint activation (hybrid).',
    'If contingent choice reading is precise: U.S. policy agency is the primary causal variable. If the distinction collapses: the reading and hybrid reading are describing the same phenomenon with different emphasis. Consequences for institutional reform: choice-based reading implies policy redesign is primary lever; hybrid reading suggests structural reform is necessary regardless of policy choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_ambiguity, conceptual, 'Whether this reading emphasizes policy choice agency or structural trigger necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1945, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trans_cont_tr_t1950, transition_causality__contingent_choice_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(trans_cont_tr_t1960, transition_causality__contingent_choice_reading, theater_ratio, 1960, 0.22).
narrative_ontology:measurement(trans_cont_tr_t1968, transition_causality__contingent_choice_reading, theater_ratio, 1968, 0.32).
narrative_ontology:measurement(trans_cont_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.35).

% Extraction over time
narrative_ontology:measurement(trans_cont_be_t1950, transition_causality__contingent_choice_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(trans_cont_be_t1960, transition_causality__contingent_choice_reading, base_extractiveness, 1960, 0.32).
narrative_ontology:measurement(trans_cont_be_t1968, transition_causality__contingent_choice_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement(trans_cont_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(trans_cont_su_t1950, transition_causality__contingent_choice_reading, suppression_requirement, 1950, 0.15).
narrative_ontology:measurement(trans_cont_su_t1960, transition_causality__contingent_choice_reading, suppression_requirement, 1960, 0.28).
narrative_ontology:measurement(trans_cont_su_t1968, transition_causality__contingent_choice_reading, suppression_requirement, 1968, 0.4).
narrative_ontology:measurement(trans_cont_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, resource_allocation).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, triffin_dilemma__reserve_currency_contradiction).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, floating_rate_regime__stability_costs).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, petrodollar_regime__post_bretton_woods_extraction).

% DUAL FORMULATION NOTE:
% The Bretton Woods regime decomposes into three constraint stories per the ε-invariance principle: (1) bretton_woods_regime__coordination_function (ε~0.15, Rope) — the fixed-rate coordination mechanism; (2) bretton_woods_regime__extraction_mechanism (ε~0.55, Snare) — the seigniorage and monetary subordination; (3) transition_causality__contingent_choice_reading (ε~0.38, Tangled Rope) — the hybrid regime with contingent termination. This story (3) links to sibling readings of the same kernel via network.affects_constraints. Each story has distinct measurement trajectories and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, institutional, 0.08).
constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
