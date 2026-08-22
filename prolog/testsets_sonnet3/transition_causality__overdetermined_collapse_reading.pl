% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Collapse as Overdetermined Structural Inevitability (Triffin Dilemma Reading)
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the overdetermined-collapse reading of the
 *   transition_causality kernel: the Bretton Woods fixed-rate system's demise
 *   is treated as structurally inevitable because multiple independent
 *   contradictions — the Triffin Dilemma (reserve-issuance volume vs.
 *   gold-convertibility credibility), the divergence of U.S. current-account
 *   balances from reserve-currency responsibilities, and rising international
 *   capital mobility outrunning capital controls — each independently
 *   sufficed to force collapse, such that no single policy decision (not the
 *   gold pool defenses, not tighter U.S. fiscal discipline, not earlier
 *   revaluation) could have averted it once the postwar trade and
 *   capital-flow volumes materialized. The Triffin Dilemma itself is authored
 *   here as a mountain-like arithmetic constraint: a reserve currency issuer
 *   cannot simultaneously supply the world's liquidity needs and maintain a
 *   fixed gold-convertibility ratio at scale — this is treated as a
 *   structural fact about the system's design, not a policy choice, and it
 *   holds regardless of which government administered it.
 *
 * KEY AGENTS:
 *   - united_states_treasury: structural beneficiary of seigniorage as inevitable byproduct, not chosen extraction (institutional/arbitrage)
 *   - post_1971_financial_speculators: downstream beneficiaries of a collapse they did not cause (organized/arbitrage)
 *   - fixed_rate_regime_trading_partners: institutional payers with no averting lever (institutional/trapped)
 *   - developing_economy_dollar_holders: powerless payers absent from all negotiations (powerless/trapped)
 *   - gold_pool_central_banks: payers whose defense could delay but not prevent (institutional/trapped)
 *   - bretton_woods_wage_earners: powerless payers bearing stagflation-era costs (powerless/trapped)
 *   - monetary_historians: analytical observer seat, shared across all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.71).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.62).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Collapse as Overdetermined Structural Inevitability (Triffin Dilemma Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '495761d8-8306-49ec-9374-fe374bb96d80').
narrative_ontology:cs_kernel_codification('495761d8-8306-49ec-9374-fe374bb96d80', distributed).
narrative_ontology:cs_authority_grounding('495761d8-8306-49ec-9374-fe374bb96d80', distributed).
narrative_ontology:cs_reading_relation('495761d8-8306-49ec-9374-fe374bb96d80', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('495761d8-8306-49ec-9374-fe374bb96d80', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('495761d8-8306-49ec-9374-fe374bb96d80', foundational, structural_contradictions_are_independently_sufficient).
narrative_ontology:cs_axiom_status(structural_contradictions_are_independently_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('495761d8-8306-49ec-9374-fe374bb96d80', structural_contradictions_are_independently_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('495761d8-8306-49ec-9374-fe374bb96d80', secondary, counterfactual_alternative_paths_were_near_zero_viability).
narrative_ontology:cs_axiom_status(counterfactual_alternative_paths_were_near_zero_viability, holdable).
narrative_ontology:cs_axiom_grounding('495761d8-8306-49ec-9374-fe374bb96d80', counterfactual_alternative_paths_were_near_zero_viability, empirically_contingent).
narrative_ontology:cs_reference_frame('495761d8-8306-49ec-9374-fe374bb96d80', bretton_woods_1944_design_settlement).
narrative_ontology:cs_drift_state('495761d8-8306-49ec-9374-fe374bb96d80', id_1971_convertibility_suspension, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('495761d8-8306-49ec-9374-fe374bb96d80', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, post_1971_financial_speculators).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_trading_partners).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, developing_economy_dollar_holders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, gold_pool_central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, bretton_woods_wage_earners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency the system requires to expand and, under this reading, was structurally forced into persistent deficits by the Triffin Dilemma itself — the world needed more dollars for liquidity than the U.S. gold stock could ever back. Under this reading the Treasury did not choose extraction; the arithmetic of reserve-currency provision made deficit and eventual suspension of convertibility the only stable long-run path. It benefited from seigniorage as an unavoidable byproduct.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, united_states_treasury, beneficiary,
    institutional, generational, arbitrage, global).

% Profited from currency volatility once fixed rates broke, but under this reading did not cause the break — they positioned around a collapse that multiple independent contradictions (Triffin, current account divergence, gold-pool depletion, capital mobility growth) had already made structurally certain. Their gains are downstream of the inevitability, not its cause.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, post_1971_financial_speculators, beneficiary,
    organized, biographical, arbitrage, global).

% Held dollar reserves and pegged currencies to a system whose foundational contradiction — needing dollar liquidity while requiring gold-convertibility credibility — could not be resolved by any policy choice available to them. They had no lever that could have prevented the collapse; the contradiction was in the system's design, not in any party's discretion, so from this reading they bore adjustment costs they could not have averted by better bargaining.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_trading_partners, payer,
    institutional, generational, trapped, global).

% Accumulated dollar reserves as the system's designated store of value with essentially no voice in the negotiations that shaped it. When convertibility was suspended and rates floated, they absorbed valuation losses and volatility exposure they had no structural capacity to hedge against or foresee — victims of a contradiction embedded before their participation began.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, developing_economy_dollar_holders, payer,
    powerless, generational, trapped, global).

% Committed reserves to defend the London Gold Pool's fixed price against speculative pressure that was itself a symptom of the underlying Triffin contradiction. Their defense could delay but structurally could not resolve the mismatch between growing dollar liabilities and static gold backing; each defense round drew down reserves that the contradiction guaranteed would eventually be exhausted regardless of policy skill.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, gold_pool_central_banks, payer,
    institutional, biographical, trapped, continental).

% Experienced the inflationary and employment consequences of the eventual float and the stagflation years that followed, with no participation in the system's design or in the decision to suspend convertibility. Under this reading their exposure follows from a contradiction structurally built into the postwar settlement itself, not from any avoidable misstep by their governments.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, bretton_woods_wage_earners, payer,
    powerless, biographical, trapped, national).

% Analyze the collapse's causal structure retrospectively, weighing whether it was structurally overdetermined, contingent on specific decisions, or a hybrid. This reading is one of three positions this observer seat can occupy; the seat itself does not adjudicate between them.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__overdetermined_collapse_reading, diffuse).
narrative_ontology:fixing_cost_class(transition_causality__overdetermined_collapse_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system solved a genuine postwar coordination problem: providing a stable nominal anchor and mutually acceptable reserve asset so that trade and capital flows could resume after the interwar currency chaos, without requiring every state to hold gold reserves at scale.
% TRANSFER_FUNCTION: The system's terminal contradiction moved seigniorage and adjustment flexibility toward the reserve-currency issuer and its arbitrage-positioned financial actors, and moved valuation risk, reserve losses, and adjustment costs onto trading partners, reserve holders, and wage earners in the issuing and pegged economies alike — a transfer this reading holds was inherent in the system's design, not the product of any single decision.
% ABSENT_VOICES: Developing-economy dollar holders and ordinary wage earners in both the U.S. and pegged economies had no seat at Bretton Woods or in the 1971 decision to suspend convertibility; their absence means the costs of the structurally inevitable adjustment were allocated without their consent or foresight.
% DISAPPEARANCE_RATIONALE: If the Triffin contradiction had not existed — i.e., if reserve-currency issuance and gold-convertibility credibility were not structurally incompatible at the volumes required by postwar trade growth — the fixed-rate regime could plausibly have persisted, and the entire subsequent architecture of floating rates, Eurodollar markets, and modern reserve-currency seigniorage would not have taken its actual form. The overdetermination claim is precisely that removing any single contributing contradiction (Triffin alone, or current-account divergence alone, or capital mobility alone) would not have prevented collapse, because the others were independently sufficient — but removing the joint structural pattern would have produced a different monetary world entirely.
% FOUNDING_PROBLEM: The Bretton Woods system was built to prevent a repeat of 1930s competitive devaluation and monetary chaos by fixing exchange rates to a gold-backed dollar, giving the postwar trading system a stable nominal anchor.
% FOUNDING_PROBLEM_CORROBORATION: IMF historical staff papers and multiple national central bank retrospectives (outside the U.S. Treasury's own institutional narrative) attest that the specific 1944 anchor arrangement became structurally unsustainable by the late 1960s independent of any single government's preferences — the founding problem of postwar nominal stability was addressed, but the specific fixed-dollar-gold mechanism chosen to solve it was not the one that persisted; floating rates and later coordination mechanisms replaced it.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, ExtMetricName, E),
    domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(transition_causality__overdetermined_collapse_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises through the period (0.22 to 0.74) tracking the widening gap between dollar liabilities and gold backing — under this reading this rise is not evidence of a choice to extract more but evidence of an arithmetic constraint tightening as trade volume grew, exactly as a mountain constraint would show increasing binding pressure over time without anyone 'deciding' to bind harder. Theater ratio rises toward 1971 (0.42) reflecting the gold pool's increasingly performative defense operations — announcements and coordinated interventions that could not change the underlying arithmetic. Accessibility collapse is authored high (0.82) because this reading holds that once the postwar trade and capital-flow volumes were in place, no policy path back to sustainable fixed convertibility existed — the alternatives had already collapsed structurally, not just politically. Resistance is authored moderate-low (0.35) because the mountain framing implies resistance is directed at symptoms (speculative attacks, gold pool defense) rather than at a contestable choice — there was no adversary to resist, only arithmetic to discover.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. Treasury is coded as beneficiary because it collects seigniorage, but the override-worthy nuance is that this reading treats the seigniorage as a structural byproduct of the reserve role rather than as intentionally captured rent — the derivation from beneficiary declaration alone would not distinguish 'benefits because it chose to extract' from 'benefits because the system's arithmetic routes gains there regardless of choice.' Trading partners, dollar holders, gold-pool banks, and wage earners are victims by directionality because they bore adjustment costs with no structural lever to avert them — trapped exit options reflect that under the overdetermination reading, no feasible policy response existed at their level of agency, not merely that they chose not to exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The overdetermination reading resists mandatrophy mislabeling in a specific direction: it prevents treating the collapse as a governance failure that better institutions could have fixed (which would wrongly suggest the founding function could have been preserved with reform), while also preventing treating it as pure extraction by a single culpable actor (which would wrongly suggest a villain rather than a structural limit). The founding_problem_status is 'dead' because the specific 1944 mechanism could not survive, but the underlying coordination need it served (a stable nominal anchor) was NOT dead — it was addressed by successor arrangements (floating rates, later coordination mechanisms), which is why the disappearance_verdict is 'world_rearranges' rather than 'world_unchanged': removing the joint contradiction pattern would have produced a genuinely different monetary architecture, not merely a delayed version of the same one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    overdetermination_vs_single_sufficient_cause,
    'Were the Triffin Dilemma, current-account divergence, and capital-mobility growth genuinely INDEPENDENTLY sufficient causes of collapse (true overdetermination), or was one of them the actual binding constraint with the others as amplifiers — which would make this reading''s ''multiple reinforcing contradictions'' claim an overstatement of a simpler single-cause structure?',
    'Counterfactual economic modeling: simulate the system with each contradiction individually removed (e.g., slower capital mobility growth alone, holding Triffin and current-account dynamics fixed) to test whether collapse still follows on a similar timeline. If removing any single factor prevents or substantially delays collapse, true overdetermination is not established.',
    'If overdetermination fails and a single dominant cause is identified, this reading collapses toward the hybrid_trigger_reading or even contingent_choice_reading, and the mountain classification (which requires the constraint to bind regardless of any single actor''s choice) becomes harder to sustain — the classification would likely shift toward tangled_rope if a specific identifiable policy choice (e.g., U.S. fiscal expansion for Vietnam and Great Society spending) turns out to be the actual binding constraint rather than a mere amplifier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(overdetermination_vs_single_sufficient_cause, empirical, 'Whether the claimed multiple independently-sufficient causes were genuinely independent or reducible to one dominant driver.').

omega_variable(
    committer_kernel_reading_location,
    'This constraint is one of three readings (overdetermined_collapse, contingent_choice, hybrid_trigger) of the same historical transition. Where exactly does the disagreement between readings live — is it a disagreement about facts (what happened), about counterfactual reasoning (what would have happened otherwise), or about the proper unit of causal attribution (structural forces vs. individual decisions)?',
    'Comparative reading of primary-source policy deliberation records (Nixon administration internal memos, Bundesbank and Bank of England correspondence) against the structural indicators (Triffin ratio time series, capital flow volume data) to establish whether decision-makers themselves perceived live alternatives at the point of decision, which would support contingent_choice or hybrid_trigger over pure overdetermination.',
    'If decision-makers'' own contemporaneous records show they perceived genuine live alternatives (not merely retrospective rationalization), the overdetermined_collapse reading''s near-zero counterfactual viability claim is weakened, and the mountain classification here becomes a false summit — an extractive policy choice retrospectively naturalized as inevitable arithmetic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_location, conceptual, 'Locating whether the three-reading disagreement is empirical, counterfactual, or about the unit of causal attribution — and whether this affects the mountain claim''s validity.').

omega_variable(
    natural_law_vs_constructed_arithmetic,
    'Is the Triffin Dilemma a genuine mountain-grade arithmetic constraint (any reserve-currency system with these design features must eventually fail this way), or is it a constructed constraint specific to the particular gold-convertibility ratio and reserve requirements chosen in 1944 — which the U.S. Treasury benefited from treating as unavoidable rather than renegotiable?',
    'Comparative analysis of alternative reserve-asset designs proposed at the time (Keynes''s Bancor proposal, SDR precursors) that would not have exhibited the same Triffin structure — if a genuinely available 1944 alternative avoided the contradiction, the ''mountain'' framing describes a chosen design feature, not an unavoidable law of reserve currencies as such.',
    'If a viable 1944 alternative existed and was rejected for reasons that benefited the eventual reserve issuer, the beneficiary declaration on united_states_treasury would shift from ''byproduct of structural inevitability'' toward ''chosen design that was later naturalized'' — this is precisely the false-summit-mountain pattern the schema flags, and the classification would move toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_arithmetic, conceptual, 'Whether the Triffin Dilemma is a genuine mountain of reserve-currency arithmetic or a constructed feature of the specific 1944 design that benefited the eventual issuer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1944, 1976).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1944, transition_causality__overdetermined_collapse_reading, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(tran_tr_t1950, transition_causality__overdetermined_collapse_reading, theater_ratio, 1950, 0.13).
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.2).
narrative_ontology:measurement(tran_tr_t1965, transition_causality__overdetermined_collapse_reading, theater_ratio, 1965, 0.29).
narrative_ontology:measurement(tran_tr_t1971, transition_causality__overdetermined_collapse_reading, theater_ratio, 1971, 0.42).
narrative_ontology:measurement(tran_tr_t1976, transition_causality__overdetermined_collapse_reading, theater_ratio, 1976, 0.38).

% Extraction over time
narrative_ontology:measurement(tran_be_t1944, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1944, 0.22).
narrative_ontology:measurement(tran_be_t1950, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1950, 0.31).
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.44).
narrative_ontology:measurement(tran_be_t1965, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(tran_be_t1971, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1971, 0.74).
narrative_ontology:measurement(tran_be_t1976, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1976, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(transition_causality__overdetermined_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.12).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the transition_causality kernel concerning the end of Bretton Woods. Each reading authors a distinct epsilon and classification over the same historical event: overdetermined_collapse_reading (this story) claims mountain with high accessibility_collapse (0.82) and low resistance (0.35), treating the Triffin Dilemma as an arithmetic constraint that bound regardless of choice; contingent_choice_reading would claim a lower accessibility_collapse and higher resistance, treating the 1971 suspension as an avoidable policy act; hybrid_trigger_reading would sit between the two, crediting structural preconditions but requiring a contingent trigger. All three link to each other via affects_constraints because they share victim/beneficiary populations and a single historical referent, even though their epsilon values and claimed types diverge by design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
