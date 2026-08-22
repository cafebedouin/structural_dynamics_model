% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: transition_causality__contingent_choice_reading
 *   human_readable: Bretton Woods Transition as Contingent Policy Choice
 *   domain: economic/political
 *
 * SUMMARY:
 *   The Bretton Woods fixed-exchange-rate system, anchored by the U.S.
 *   commitment to convert dollars to gold at $35/ounce, faced mounting strain
 *   from 1965 onward. The U.S. was running persistent deficits to fund the
 *   Vietnam War and domestic social spending; gold reserves declined
 *   steadily; confidence in dollar convertibility eroded. By August 1971, the
 *   Nixon administration made a deliberate policy choice to suspend gold
 *   convertibility and float the dollar. This reading treats that choice as
 *   contingent: it could have been avoided or deferred with different policy
 *   decisions, negotiation strategies, or political will. The transition was
 *   not overdetermined by structural forces alone; it was actualized through
 *   deliberate U.S. policy choice that reshaped the international monetary
 *   order. The beneficiary structure centers on U.S. monetary autonomy gain
 *   and the concentration of seigniorage rents in the American financial
 *   sector. The measurement series tracks extractiveness rising sharply in
 *   1971 (the decision point) and then stabilizing as the new regime settles.
 *
 * KEY AGENTS:
 *   - us_monetary_policy_leadership: the Nixon administration and Federal Reserve, making the August 1971 decision to end gold convertibility
 *   - us_financial_sector: institutional beneficiary gaining seigniorage rents and dollar-denominated asset dominance
 *   - developing_economies: powerless payers absorbing adjustment shocks and subsequent debt crises
 *   - fixed_peg_states: moderate-power payers experiencing unilateral rule-change and currency revaluation
 *   - gold_backed_reserve_holders: powerful but exposed to wealth loss from dollar devaluation
 *   - bretton_woods_regime_architects: analytical observers whose designed system was deliberately dismantled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.68).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.71).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.59).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Bretton Woods Transition as Contingent Policy Choice").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "economic/political").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, '4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8').
narrative_ontology:cs_kernel_codification('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', distributed).
narrative_ontology:cs_authority_grounding('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', extraction).
narrative_ontology:cs_reading_relation('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', foundational, policy_decision_contingent_on_u_s_choice).
narrative_ontology:cs_axiom_status(policy_decision_contingent_on_u_s_choice, holdable).
narrative_ontology:cs_axiom_grounding('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', policy_decision_contingent_on_u_s_choice, instrumental).
narrative_ontology:cs_axiom('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', foundational, alternatives_to_unilateral_exit_remained_viable).
narrative_ontology:cs_axiom_status(alternatives_to_unilateral_exit_remained_viable, holdable).
narrative_ontology:cs_axiom_grounding('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', alternatives_to_unilateral_exit_remained_viable, empirically_contingent).
narrative_ontology:cs_reference_frame('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', bretton_woods_negotiated_regime).
narrative_ontology:cs_drift_state('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', august_1971_unilateral_exit, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('4850fe5c-8b51-4e97-bbc6-c989c7bfbcc8', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_monetary_autonomy_gain).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_financial_sector).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, developing_economies).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, fixed_peg_states).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, gold_backed_reserve_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Nixon administration and Federal Reserve made the decision on August 15, 1971 to suspend gold convertibility. They set and enforced the new floating-rate regime through bilateral negotiations and multilateral arrangements. Their choice reshaped the international monetary order and concentrated seigniorage rents on the U.S. financial system.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_monetary_policy_leadership, agenda_setter,
    institutional, generational, analytical, global).

% Major banks, investment firms, and financial institutions benefited directly: the dollar remained the global reserve currency without the gold constraint; dollar-denominated assets became the inflation hedge; credit expansion became unconstrained by specie-backing; seigniorage rents accrued to the U.S. financial system instead of being distributed through gold flows to other central banks.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_financial_sector, beneficiary,
    institutional, generational, arbitrage, global).

% States that had pegged currencies to the dollar (Western Europe, Japan, Canada, others) experienced a sudden, unilateral rule-change. Their central banks absorbed massive losses on dollar reserves, export competitiveness shifted unpredictably, and they had to choose between floating, revaluing, or imposing capital controls—none without severe adjustment costs. The decision was made without their consent or negotiation.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, fixed_peg_states, payer,
    moderate, generational, constrained, global).

% Absorbed the heaviest costs: currency instability made import pricing volatile, dollar reserves lost value, debt-service burdens shifted with exchange-rate movements, and subsequent inflation and Volcker's interest-rate hikes triggered sovereign-debt crises across Latin America and Africa through the 1980s. They had no seat at the decision table and no exit options (dependent on U.S. trade and financing).
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, developing_economies, payer,
    powerless, generational, trapped, global).

% Central banks and international institutions holding dollar reserves experienced direct wealth loss: gold backing evaporated overnight, reserve-asset values became volatile, and inflation eroded purchasing power. Unlike developing economies, they had some mobility (could diversify into other currencies), but the core loss was imposed by U.S. policy choice. Their powerful position gave them voice in negotiations but not veto power over the unilateral decision.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, gold_backed_reserve_holders, payer,
    powerful, biographical, mobile, global).

% The IMF and World Bank were not consulted meaningfully before the August 1971 decision. They had to adapt their institutional frameworks to floating rates and new volatility. Their original mandate assumed fixed-peg stability; the transition forced a redesign they did not choose and had to implement under pressure from the new U.S.-led floating-rate regime.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, international_monetary_institutions, excluded,
    institutional, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__contingent_choice_reading, us_financial_sector).
narrative_ontology:fixing_cost_class(transition_causality__contingent_choice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bretton Woods established stable exchange rates, predictable gold-to-dollar conversion at $35/ounce, and a single global reserve currency to coordinate international trade and capital flows. The transition from fixed to floating rates dissolved this coordination mechanism. The contingent-choice reading frames this as a problem that could have been solved through negotiated reform rather than unilateral exit.
% TRANSFER_FUNCTION: The decision transferred purchasing power from reserve-holding nations and fixed-peg states to the U.S. financial sector: the dollar lost gold backing but retained dominance; developing economies and fixed-peg states absorbed adjustment shocks; seigniorage rents accrued to Washington; volatility redistributed gains from importers to currency speculators (disproportionately U.S.-based).
% ABSENT_VOICES: Developing economies, IMF members without veto power, fixed-peg-currency states, and alternative monetary architects (advocates of Keynes's bancor, reformed gold-exchange standards, or symmetric adjustment mechanisms) were not consulted before the decision. Their objections came only after August 1971, when the choice was already fait accompli. Had they been at the negotiation table earlier, a different arrangement might have emerged.
% DISAPPEARANCE_RATIONALE: If the Nixon administration had chosen differently—negotiating a reformed Bretton Woods, extending the gold-exchange standard with capital controls, or adopting a Keynes-style international currency—the entire structure of 1970s–1980s international finance would have evolved differently. Floating rates, petrodollars, dollar hegemony, emerging-market debt crises, the structure of modern asset markets: all are contingent on the August 1971 decision. Without that choice, alternative monetary architectures would have persisted or emerged.
% FOUNDING_PROBLEM: Post-WWII, Bretton Woods attempted to preserve gold backing while allowing national monetary autonomy. By 1965–1971, Triffin's dilemma became acute: the U.S. had run persistent deficits (Vietnam War, Great Society spending); gold reserves declined; confidence in dollar convertibility eroded. The founding problem was how to sustain a global monetary order under conflicting demands—fixed rates vs. independent monetary policy, gold backing vs. deficit financing.
% FOUNDING_PROBLEM_CORROBORATION: Economists outside the U.S. policy apparatus (Triffin, Hirsch, Mundell) documented Bretton Woods' internal contradictions from independent positions. Bretton Woods' own architects (Keynes, White) had foreseen instability in the original design. However, U.S. policy officials (Nixon, Kissinger, Federal Reserve leadership) framed the problem as requiring unilateral exit rather than collective negotiation or reform. The scholarly record and non-U.S. government positions support the framing that the problem was soluble through negotiated reform, making the chosen exit strategy contingent and not necessary.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.31 in 1965 to 0.52 by August 1971 (the decision point) and climbs to 0.68 by 1973 (full float adoption). This trajectory reflects the measured extraction of the new floating-rate regime: seigniorage concentration, capital-flow volatility that harms emerging markets, and the shift of adjustment burdens from the U.S. to other economies. Suppression requirement rises in parallel (0.42 to 0.71) because maintaining dollar dominance in a world of floating rates required active enforcement—capital controls, swap arrangements, IMF conditions, military/political pressure on alternatives (the petrodollar recycling mechanism). Theater ratio is moderate (0.42): the justifications are real (reserve recycling, monetary discipline) but a growing share of the enforcement machinery defends seigniorage extraction rather than genuine coordination. The shared measurement grid runs across all three metrics at each time point (1965, 1970, 1971, 1973, 1979, 1985) so the temporal dynamics are visible: the phase transition at 1971 is the decision point itself; post-1973 stabilization reflects the new regime bedding in.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (U.S. policy leadership) experiences the transition as freeing up monetary autonomy constrained by an obsolete gold standard—a rope or even coordination gain (escape from an impossible commitment). The financial-sector beneficiary experiences it as seigniorage capture (tangled rope to snare, depending on the seat). Developing-economy payers experience it as sudden, unilateral extraction—a pure snare (no coordination function accrues to them; all adjustment costs are theirs). Fixed-peg states experience it as institutional betrayal: they had negotiated fixed parities and built economic relationships on that stability; the U.S. unilaterally broke the pact. The engine computes these per-seat classifications from power, exit, and the beneficiary/victim structure; the authored claim (tangled_rope) represents the constraint's mixed character at the global level: genuine coordination problem solved (Bretton Woods was breaking down), but the solution concentrated rents and asymmetrically extracted from the least powerful parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. monetary leadership sits at the full-agenda-setter pole (d ≈ 0.0, subsidy/autonomy gain). The U.S. financial sector is the primary beneficiary (d ≈ 0.1, concentrates seigniorage rents). Developing economies sit at the full-target pole (d ≈ 1.0): powerless, trapped, absorb the Volcker shocks and debt crises of the 1980s. Fixed-peg states occupy a middle position (d ≈ 0.65): they have moderate power and some mobility but faced sudden rule-change and adjustment burdens. Gold-backed reserve holders have powerful positions but are partly captured by the dollar system's new dominance (d ≈ 0.5, symmetric between seigniorage gain and reserve devaluation). This heterogeneous directionality is the key to the measured seat divergence: from Washington's seat this is a justified autonomy gain; from developing-economy seats it is pure extraction; from reserve-holder seats it is mixed (trapped upside, exposed downside).
 *
 * MANDATROPHY ANALYSIS:
 *   Under the contingent-choice reading, Bretton Woods is not mandatrophy: the founding problem (sustaining fixed rates under Triffin's dilemma) was still live and contested at the time of the transition. The U.S. could have negotiated a reformed gold-exchange standard, adopted Keynes's bancor proposal, or committed to discipline under a genuinely symmetric adjustment mechanism. The choice to end it unilaterally was political, not mandated by exhaustion of alternatives. This distinguishes the reading from the overdetermined-collapse reading, which treats mandatrophy as the dominant pathway: under overdetermined, the founding problem becomes dead (structural contradictions are irresolvable) and the transition is merely the actualization of inevitable collapse. Under contingent-choice, the problem remains live and the transition is a discretionary governance choice—which means alternatives existed and could have been chosen.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_viability_of_reform,
    'Would a reformed Bretton Woods system (preserving fixed rates with asymmetric adjustment or Keynes''s bancor proposal) have been politically and economically viable if the U.S. had committed to negotiate rather than unilaterally exit in August 1971?',
    'Archival evidence from U.S. Treasury, Federal Reserve, and State Department records from 1969–1971 on what reform proposals were considered and rejected; counterfactual analysis of growth trajectories and capital-flow stability under alternative monetary regimes in the 1970s.',
    'If reform was viable, the contingent-choice reading is strengthened: the transition was a genuine policy choice, not a forced response. If reform was not viable (structural contradictions foreclosed alternatives), the overdetermined-collapse or hybrid-trigger readings gain weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_of_reform, empirical, 'Whether alternative monetary arrangements were structurally possible or only temporarily delayed the inevitable collapse').

omega_variable(
    causal_role_of_nixon_decision,
    'How much of the transition''s timing and form (sudden float vs. negotiated transition) was determined by the August 1971 Nixon decision itself, versus by underlying structural forces that would have forced a transition regardless of the specific decision?',
    'Counterfactual historical modeling: simulate a world in which the U.S. negotiated extended gold support (e.g., two-tier gold market, expanded SDR use, capital controls) and track whether floating rates and dollar instability would have emerged by 1973 anyway, or whether a reformed system could have persisted through the 1970s.',
    'If the decision was causal (changing the specific form and timing of the transition), the contingent-choice reading is supported. If underlying forces would have produced floating rates by 1973 regardless, the reading''s claim to contingency weakens and hybrid or overdetermined readings gain ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_role_of_nixon_decision, conceptual, 'The degree to which the August 1971 policy decision was the primary causal force versus one actualization of inevitable structural collapse').

omega_variable(
    beneficiary_structure_under_alternative_outcomes,
    'If the Bretton Woods transition had been negotiated (rather than unilateral) or deferred through a reformed fixed-rate system, who would have benefited and who would have paid in the alternative outcome?',
    'Game-theoretic analysis of negotiated-reform scenarios; historical precedent from other international monetary transitions (e.g., ERM adjustments, Smithsonian Agreement negotiations); interviews with surviving policy participants and economists on what they believed would have happened under negotiated reform.',
    'If beneficiary structure under negotiated reform would have been more symmetric (U.S. still gains some autonomy, but developing economies bear lower costs), the extraction characterized in this reading is contingent on the unilateral choice-making and might be partly avoidable. If beneficiary structure is similar under all plausible outcomes, the extraction is more structural and less dependent on the specific decision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_under_alternative_outcomes, empirical, 'Whether the asymmetric extraction benefiting the U.S. was inherent to any monetary transition or specific to the unilateral choice made').

omega_variable(
    kernel_framing_under_determination,
    'Is the transition a decision that could have been avoided (contingent_choice framing), or is it the actualization of structural inevitability with specific trigger events (hybrid_trigger or overdetermined framing)?',
    'The three readings of the transition_causality kernel represent genuinely incommensurable causal framings. This omega documents that the reading chosen here (contingent_choice) is one framing among three; alternative framings would produce different structural diagnoses (different policy actors as causal agents, different beneficiary structures, different mandatrophy status). No single empirical fact resolves this—it depends on the causal framework adopted (intentionalist vs. structuralist historiography, agency vs. constraints).',
    'If the contingent_choice reading is adopted, the transition is a policy choice with alternatives; mandatrophy is not resolved (the founding problem remains live; the arrangement could have been reformed). If the overdetermined reading is adopted, the transition is inevitable; mandatrophy is resolved (structural contradictions foreclose alternatives). If the hybrid reading is adopted, structural contradictions set the stage but specific trigger events could have been avoided, producing intermediate contingency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'The fundamental under-determination of the kernel across the three sibling readings; this reading''s causal attribution is one framing among contending narratives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 1965, 1985).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1965, transition_causality__contingent_choice_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(tran_tr_t1970, transition_causality__contingent_choice_reading, theater_ratio, 1970, 0.28).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__contingent_choice_reading, theater_ratio, 1971, 0.31).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__contingent_choice_reading, theater_ratio, 1973, 0.39).
narrative_ontology:measurement(tran_tr_t1979, transition_causality__contingent_choice_reading, theater_ratio, 1979, 0.42).
narrative_ontology:measurement(tran_tr_t1985, transition_causality__contingent_choice_reading, theater_ratio, 1985, 0.42).

% Extraction over time
narrative_ontology:measurement(tran_be_t1965, transition_causality__contingent_choice_reading, base_extractiveness, 1965, 0.31).
narrative_ontology:measurement(tran_be_t1970, transition_causality__contingent_choice_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(tran_be_t1971, transition_causality__contingent_choice_reading, base_extractiveness, 1971, 0.52).
narrative_ontology:measurement(tran_be_t1973, transition_causality__contingent_choice_reading, base_extractiveness, 1973, 0.64).
narrative_ontology:measurement(tran_be_t1979, transition_causality__contingent_choice_reading, base_extractiveness, 1979, 0.68).
narrative_ontology:measurement(tran_be_t1985, transition_causality__contingent_choice_reading, base_extractiveness, 1985, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t1965, transition_causality__contingent_choice_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement(tran_su_t1970, transition_causality__contingent_choice_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(tran_su_t1971, transition_causality__contingent_choice_reading, suppression_requirement, 1971, 0.61).
narrative_ontology:measurement(tran_su_t1973, transition_causality__contingent_choice_reading, suppression_requirement, 1973, 0.68).
narrative_ontology:measurement(tran_su_t1979, transition_causality__contingent_choice_reading, suppression_requirement, 1979, 0.71).
narrative_ontology:measurement(tran_su_t1985, transition_causality__contingent_choice_reading, suppression_requirement, 1985, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__contingent_choice_reading, 0.18).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__overdetermined_collapse_reading).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'transition_causality.' The kernel is the historical event of the Bretton Woods transition (August 1971). Three structurally distinct constraint stories instantiate three different readings: contingent_choice_reading (this file — the transition was a discretionary policy decision that could have been avoided), overdetermined_collapse_reading (the transition was structurally inevitable), and hybrid_trigger_reading (structural contradictions plus specific trigger events). Each reading has different beneficiary structures, different ε values, different mandatrophy diagnoses, and different implications for causality and responsibility. They share the referent (the August 1971 event) but the causal frameworks are incommensurable; no single empirical fact uniquely determines which reading is true. See commentary.kernel_context and cs_structure.reading_relations for the detailed comparative structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, powerless, 0.95).
constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
