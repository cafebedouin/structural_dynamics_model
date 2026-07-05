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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Bretton Woods Collapse as Overdetermined Structural Inevitability
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This story instantiates the overdetermined_collapse_reading of the
 *   transition_causality kernel: the claim that the 1971-73 breakdown of the
 *   Bretton Woods fixed-rate gold-dollar system was not a contingent policy
 *   failure but a structurally foreclosed outcome of multiple independently
 *   sufficient contradictions converging — the Triffin Dilemma (the reserve
 *   issuer must run persistent deficits to supply world liquidity while those
 *   same deficits erode confidence in convertibility), the impossible trinity
 *   (fixed rates, free capital flows, and independent monetary policy cannot
 *   coexist), and the widening gap between U.S. gold stock and foreign-held
 *   dollar claims. Under this reading, Nixon's August 1971 gold-window
 *   closure is the moment the foreclosure became visible, not the moment it
 *   was decided. This is one of three sibling constraint stories sharing the
 *   transition_causality kernel; the contingent_choice_reading treats the
 *   same historical episode as an avoidable policy decision, and the
 *   hybrid_trigger_reading treats it as structural fragility requiring a
 *   contingent trigger. Each sibling has its own epsilon, beneficiary/victim
 *   structure, and classification — they are not different measurements of
 *   one constraint.
 *
 * KEY AGENTS:
 *   - United States Treasury/Federal Reserve: primary beneficiary of dollar's exorbitant privilege surviving the transition (institutional/global) — retains reserve-currency seigniorage without gold discipline
 *   - Post-1971 financial intermediaries: beneficiaries of floating-rate arbitrage and expanded capital mobility (organized/global)
 *   - Fixed-rate regime dependent exporters: bear transition costs via currency volatility exposure they had not priced (moderate/national) — primary victims
 *   - Foreign central banks holding dollar reserves (esp. Bank of France, Bundesbank): bear direct loss on dollar reserve valuation and lose the convertibility guarantee they had relied on (institutional/national) — victims with some negotiating power
 *   - Smaller pegging economies: bear the compounded shock with least capacity to hedge or renegotiate (powerless/national) — most exposed victims
 *   - Analytical observer (this constraint's own vantage): sees the convergence of Triffin, impossible-trinity, and gold-ratio pressures as jointly foreclosing the pre-1971 equilibrium
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.61).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.55).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Collapse as Overdetermined Structural Inevitability").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'de749938-d678-4e41-bb3b-3e028479ab24').
narrative_ontology:cs_kernel_codification('de749938-d678-4e41-bb3b-3e028479ab24', distributed).
narrative_ontology:cs_authority_grounding('de749938-d678-4e41-bb3b-3e028479ab24', distributed).
narrative_ontology:cs_reading_relation('de749938-d678-4e41-bb3b-3e028479ab24', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('de749938-d678-4e41-bb3b-3e028479ab24', transition_causality__hybrid_trigger_reading, influences).
narrative_ontology:cs_axiom('de749938-d678-4e41-bb3b-3e028479ab24', foundational, structural_overdetermination_of_regime_collapse).
narrative_ontology:cs_axiom_status(structural_overdetermination_of_regime_collapse, holdable).
narrative_ontology:cs_axiom_grounding('de749938-d678-4e41-bb3b-3e028479ab24', structural_overdetermination_of_regime_collapse, empirically_contingent).
narrative_ontology:cs_axiom('de749938-d678-4e41-bb3b-3e028479ab24', secondary, counterfactual_reform_paths_were_jointly_foreclosed).
narrative_ontology:cs_axiom_status(counterfactual_reform_paths_were_jointly_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('de749938-d678-4e41-bb3b-3e028479ab24', counterfactual_reform_paths_were_jointly_foreclosed, empirically_contingent).
narrative_ontology:cs_reference_frame('de749938-d678-4e41-bb3b-3e028479ab24', bretton_woods_fixed_convertibility_regime).
narrative_ontology:cs_drift_state('de749938-d678-4e41-bb3b-3e028479ab24', smithsonian_agreement_collapse_1973, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('de749938-d678-4e41-bb3b-3e028479ab24', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, united_states_treasury).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, post1971_financial_intermediaries).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_dependent_exporters).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, foreign_central_banks_holding_dollar_reserves).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, smaller_pegging_economies).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, impossible_trinity_theorem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the reserve currency and ultimately makes the unilateral decision to close the gold window in August 1971, ending direct convertibility. Retains dollar seigniorage and monetary policy independence after the transition without the gold-stock discipline that had constrained it, while framing the decision as a forced response to structural pressures beyond its control.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, united_states_treasury, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, united_states_treasury, agenda_setter).

% Gain an entirely new market in floating-currency trading, hedging instruments, and arbitrage opportunities that did not exist under fixed rates. Have no formal role in the transition decision but structurally profit from the volatility the new regime introduces.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, post1971_financial_intermediaries, beneficiary,
    organized, biographical, arbitrage, global).

% Priced export contracts and investment decisions on the assumption of stable exchange rates under the fixed regime. Absorb sudden currency volatility and repricing costs they had no mechanism to hedge against before the transition, with limited capacity to renegotiate existing contracts.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_dependent_exporters, payer,
    moderate, biographical, constrained, national).

% Hold substantial dollar reserves accumulated in reliance on the gold-convertibility guarantee. Press for reform (SDR creation, revaluation coordination) throughout the 1960s but are structurally unable to force U.S. fiscal or monetary discipline; absorb the valuation shock when convertibility ends, though with some capacity to negotiate the Smithsonian realignment.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, foreign_central_banks_holding_dollar_reserves, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, foreign_central_banks_holding_dollar_reserves, excluded).

% Pegged their currencies to the dollar for trade-settlement stability with essentially no seat at the G10 negotiations that managed the transition's terms. Absorb the compounded shock of the regime shift with the thinnest reserve buffers and least capacity to hedge or renegotiate exposure.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, smaller_pegging_economies, payer,
    powerless, biographical, trapped, national).

% Central bankers and finance ministries (notably France, West Germany) who proposed reserve-system reforms throughout the 1960s to head off the collapse. Their proposals are heard in international forums but not adopted with sufficient speed or scope to alter the outcome under this reading — this reading holds their exclusion from decisive influence was itself structurally determined, not merely a political failure.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, european_reform_advocates, excluded,
    powerful, biographical, constrained, continental).

% Reconstruct the causal structure of the transition after the fact, including Triffin's own contemporaneous warnings (published 1960) that the dilemma bearing his name would force exactly this outcome absent structural reform of the reserve system.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, monetary_economics_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The pre-1971 Bretton Woods system solved a genuine post-war coordination problem: providing a stable, trusted, liquid reserve asset (the gold-backed dollar) so that international trade and investment could be priced and settled without the volatility of fully floating exchange rates or the deflationary rigidity of a pure gold standard.
% TRANSFER_FUNCTION: The transition moved seigniorage privilege and monetary-policy autonomy toward the United States and toward financial intermediaries able to exploit new floating-rate markets, while moving realized currency risk and reserve-valuation losses onto foreign central banks and, disproportionately, onto smaller economies with pegged currencies and thin reserve buffers.
% ABSENT_VOICES: Smaller pegging economies and their citizens had essentially no seat at the G10 and Smithsonian negotiations that managed the transition's terms; European reform advocates had a seat but insufficient leverage to alter the structural trajectory under this reading. Both groups would argue for a coordinated, negotiated reserve-system reform rather than a unilateral U.S. decision followed by improvised realignment.
% DISAPPEARANCE_RATIONALE: Under this reading, the pre-1971 fixed-rate arrangement could not have persisted regardless of any single party's preference — its disappearance was not a choice to be reversed but a foreclosure to be recognized. Whether 'the world rearranges' if this structural-inevitability reading itself were false is exactly the question the sibling readings dispute: the contingent_choice_reading holds the world would look different (continued convertibility) under different policy choices, which this reading denies was ever a live option after the mid-1960s.
% FOUNDING_PROBLEM: Providing a stable, liquid, trusted international reserve asset to support post-war trade reconstruction and prevent competitive devaluation spirals of the kind that worsened the interwar depression.
% FOUNDING_PROBLEM_CORROBORATION: Robert Triffin himself, an economist external to any government's beneficiary interest, published the structural critique bearing his name in 1960, warning from outside both the U.S. Treasury and the foreign central banks that the dual role of the dollar as national currency and world reserve asset was structurally unsustainable. Subsequent IMF and BIS historical analyses, produced by staff economists rather than by the U.S. Treasury or reserve-holding central banks directly, corroborate that the founding problem of reserve-asset provision was resolved by 1973 through a different (floating-rate) mechanism, not preserved.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, contested).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.61 at interval end because the transition, however structurally driven, redistributed real costs: dollar-reserve holders absorbed valuation losses and smaller economies absorbed volatility they had no mechanism to hedge, while the reserve issuer retained privilege. Suppression is moderate (0.55) — the pre-1971 system's exit options for peg-dependent economies were genuinely constrained (leaving the peg meant forfeiting trade-settlement infrastructure), but this is less an actively enforced suppression than a structural lock-in. Accessibility collapse is authored high (0.88) reflecting the near-zero counterfactual viability claimed by this reading: once the impossible trinity and Triffin Dilemma were both binding, alternative fixed-rate paths had genuinely collapsed, not merely become inconvenient. Resistance is authored low (0.22) because the system's core parties (major central banks) largely accommodated the transition once its inevitability became apparent rather than mounting sustained structural resistance — the theater_ratio trajectory (0.12->0.28) reflects growing diplomatic performance (G10 negotiations, Smithsonian Agreement) that increasingly functioned as face-saving ritual around an already-foreclosed outcome rather than genuine renegotiation capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   The United States Treasury and post-1971 financial intermediaries are coded as beneficiaries: the dollar retained reserve-currency status and seigniorage benefits without the gold-convertibility discipline that had constrained U.S. fiscal and monetary policy, and financial intermediaries gained a new floating-rate arbitrage market. Foreign central banks holding dollar reserves, fixed-rate-dependent exporters, and smaller pegging economies are victims: they bore the realized costs of a regime shift they had structurally little power to prevent or fully hedge against, with smaller economies bearing the most concentrated exposure given minimal negotiating leverage and thinnest reserve buffers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a stable, liquid international reserve asset to support post-war trade reconstruction — is genuinely dead by any coherent read: no fixed-gold-dollar convertibility mechanism has existed since 1973, and the floating-rate successor regime solves (imperfectly) the same coordination problem through different means. This reading treats that death as structurally forced rather than negotiated, which forecloses reading the pre-1971 defenders (European central bankers pressing for reform) as having had a genuine live option to prevent collapse — they could at most have altered the timing and manner of an already-foreclosed transition, not its occurrence. This is the core empirical/conceptual wager this reading makes against its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'Is the Bretton Woods transition better modeled as (a) an overdetermined mountain-like collapse where the Triffin Dilemma and the impossible trinity jointly foreclosed any stable fixed-rate path, (b) a contingent policy choice (Nixon''s August 1971 decision) that could have been substituted with coordinated reserve reform, or (c) a hybrid where structural contradiction created a fragile regime that nonetheless required a contingent trigger (speculative attacks, the 1971 gold run) to actualize collapse?',
    'Counterfactual institutional analysis: would proposed 1960s reform alternatives (SDR creation timing, a Bergsten-style substitution account, coordinated European revaluation) have been jointly sufficient to preserve convertibility past 1971 absent the specific 1971 trigger events? If multiple independent reform paths were foreclosed by the same underlying contradiction (dollar liquidity vs. dollar confidence), overdetermination is supported; if a single early intervention would plausibly have sufficed, contingency is supported.',
    'Determines which of three sibling constraint stories (this overdetermined reading, the contingent_choice_reading, or the hybrid_trigger_reading) is the historically operative one — each implies a different victim structure and a different verdict on whether policymakers of the era bear causal responsibility or were executing an already-foreclosed transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-axis ambiguity: this constraint instantiates the overdetermined_collapse_reading of the transition_causality kernel; sibling readings (contingent_choice_reading, hybrid_trigger_reading) are separate constraint stories, not alternative measurements of this one.').

omega_variable(
    fsm_triffin_naturality_vs_construction,
    'Is the Triffin Dilemma a genuine mathematical/logical necessity of any single-reserve-currency fixed-rate system (a mountain), or is its apparent inevitability partly a constructed narrative that retrospectively benefits the United States by framing a policy choice (unilateral gold-window closure) as forced by nature rather than chosen for advantage?',
    'Formal modeling of alternative reserve-currency architectures (multi-currency reserve baskets, SDR-centered systems proposed contemporaneously by Triffin himself as a fix) to determine whether the dilemma is inherent to ANY fixed single-currency-reserve system or was specific to the particular institutional choices made in 1944 and defended afterward by the reserve-currency issuer.',
    'If genuinely a mountain, the beneficiary declarations here are incidental fallout of a natural-law constraint and the FSM signature should not fire on reclassification grounds even though beneficiaries are present. If partly constructed, the mountain claim understates U.S. agency and the constraint should trend toward tangled_rope on reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fsm_triffin_naturality_vs_construction, conceptual, 'False-summit candidate: mountain claim carries declared beneficiaries (US Treasury, post-1971 financial intermediaries), requiring explicit adjudication of natural-law vs. constructed status.').

omega_variable(
    counterfactual_viability_measurement,
    'How close to zero was the actual counterfactual viability of continued fixed-rate convertibility past 1971, given simultaneous pressures from Vietnam War deficit spending, European reserve accumulation, and the structural dollar-gold ratio?',
    'Quantitative reconstruction of U.S. gold reserves vs. foreign-held dollar claims trajectory from 1958-1971, cross-referenced against any historically documented reform proposal that had realistic political viability in the relevant legislatures and central banks.',
    'A near-zero counterfactual viability estimate strongly supports overdetermination (this reading); any surviving viable reform path with real political traction weakens the mountain claim toward the hybrid or contingent readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_viability_measurement, empirical, 'Empirical test of the ''near-zero counterfactual viability'' delta claimed for this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 1958, 1973).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t1958, transition_causality__overdetermined_collapse_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(tran_tr_t1961, transition_causality__overdetermined_collapse_reading, theater_ratio, 1961, 0.15).
narrative_ontology:measurement(tran_tr_t1964, transition_causality__overdetermined_collapse_reading, theater_ratio, 1964, 0.19).
narrative_ontology:measurement(tran_tr_t1967, transition_causality__overdetermined_collapse_reading, theater_ratio, 1967, 0.23).
narrative_ontology:measurement(tran_tr_t1970, transition_causality__overdetermined_collapse_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement(tran_tr_t1973, transition_causality__overdetermined_collapse_reading, theater_ratio, 1973, 0.28).

% Extraction over time
narrative_ontology:measurement(tran_be_t1958, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1958, 0.32).
narrative_ontology:measurement(tran_be_t1961, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1961, 0.38).
narrative_ontology:measurement(tran_be_t1964, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1964, 0.44).
narrative_ontology:measurement(tran_be_t1967, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1967, 0.51).
narrative_ontology:measurement(tran_be_t1970, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement(tran_be_t1973, transition_causality__overdetermined_collapse_reading, base_extractiveness, 1973, 0.61).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(transition_causality__overdetermined_collapse_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(transition_causality__overdetermined_collapse_reading, 0.2).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, transition_causality__hybrid_trigger_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints sharing the transition_causality kernel over the same historical episode (Bretton Woods collapse, 1958-1973). Each sibling assigns a different causal structure to the same event and therefore a different epsilon and victim/beneficiary structure: this reading (overdetermined_collapse_reading) treats the transition as jointly foreclosed by structural contradictions with near-zero counterfactual viability (high accessibility_collapse, mountain-leaning classification with FSM scrutiny given declared beneficiaries); contingent_choice_reading treats it as an avoidable 1971 policy decision (lower accessibility_collapse, higher resistance, more agency attributed to U.S. policymakers); hybrid_trigger_reading treats structural contradiction as necessary-but-insufficient, requiring a contingent trigger event to actualize collapse (intermediate accessibility_collapse). The three are linked via network.affects_constraints in all three files rather than merged into one constraint with a variable causal-attribution parameter, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
