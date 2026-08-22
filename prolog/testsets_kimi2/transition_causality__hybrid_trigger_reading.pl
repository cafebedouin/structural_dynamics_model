% ============================================================================
% CONSTRAINT STORY: transition_causality__hybrid_trigger_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: transition_causality__hybrid_trigger_reading
 *   human_readable: Bretton Woods Hybrid Trigger Transition
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   The Bretton Woods system (1944-1971) established a dollar-gold exchange
 *   standard administered by the US Treasury and IMF. This constraint story
 *   authors the system as read through the hybrid_trigger reading of the
 *   transition_causality kernel: the Triffin Dilemma created a slow-burning
 *   structural contradiction between global dollar liquidity demand and gold
 *   convertibility confidence, yet the collapse actualized only through
 *   contingent triggers, specifically the Vietnam War fiscal shock and
 *   French-led gold runs. The constraint coordinated post-war monetary
 *   stability while extracting asymmetric seigniorage and adjustment costs
 *   from non-reserve nations. This is one reading of a contested kernel;
 *   sibling readings include the contingent_choice reading (avoidable policy
 *   errors) and the overdetermined_collapse reading (structural inevitability
 *   independent of triggers).
 *
 * KEY AGENTS:
 *   - us_reserve_issuer: Primary beneficiary and agenda-setter (institutional/arbitrage) â collects seigniorage and asymmetric policy autonomy, administers the gold window.
 *   - non_reserve_currency_nations: Primary payer (organized/constrained) â bear asymmetric balance-of-payments adjustment, imported inflation, and austerity costs.
 *   - gold_window_claimants: Secondary payer (powerful/mobile) â dollar reserve holders who exercise exit via gold conversion but bear convertibility suspension risk.
 *   - international_trade_sector: Secondary beneficiary (powerful/mobile) â gains from stable exchange rates and predictable settlement.
 *   - imf_enforcement_apparatus: Agenda-setter (institutional/constrained) â enforces fixed parities and conditional lending.
 *   - academic_observers: Observer (analytical/analytical) â diagnose systemic contradictions such as the Triffin Dilemma.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, 0.78).
domain_priors:suppression_score(transition_causality__hybrid_trigger_reading, 0.76).
domain_priors:theater_ratio(transition_causality__hybrid_trigger_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(transition_causality__hybrid_trigger_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__hybrid_trigger_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__hybrid_trigger_reading, "Bretton Woods Hybrid Trigger Transition").
narrative_ontology:topic_domain(transition_causality__hybrid_trigger_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__hybrid_trigger_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__hybrid_trigger_reading, '121cdddc-a976-4ab9-9c3e-368d58773c56').
narrative_ontology:cs_kernel_codification('121cdddc-a976-4ab9-9c3e-368d58773c56', fixed_text).
narrative_ontology:cs_authority_grounding('121cdddc-a976-4ab9-9c3e-368d58773c56', lineage).
narrative_ontology:cs_interpretation_layer_present('121cdddc-a976-4ab9-9c3e-368d58773c56').
narrative_ontology:cs_reading_relation('121cdddc-a976-4ab9-9c3e-368d58773c56', transition_causality__contingent_choice_reading, coexists_with).
narrative_ontology:cs_reading_relation('121cdddc-a976-4ab9-9c3e-368d58773c56', transition_causality__overdetermined_collapse_reading, forecloses).
narrative_ontology:cs_axiom('121cdddc-a976-4ab9-9c3e-368d58773c56', foundational, contingent_trigger_necessity).
narrative_ontology:cs_axiom_status(contingent_trigger_necessity, holdable).
narrative_ontology:cs_axiom_grounding('121cdddc-a976-4ab9-9c3e-368d58773c56', contingent_trigger_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('121cdddc-a976-4ab9-9c3e-368d58773c56', managed_adjustable_peg_equilibrium).
narrative_ontology:cs_drift_state('121cdddc-a976-4ab9-9c3e-368d58773c56', post_vietnam_fiscal_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('121cdddc-a976-4ab9-9c3e-368d58773c56', '').
narrative_ontology:cs_kernel_id(transition_causality__hybrid_trigger_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, us_reserve_issuer).
narrative_ontology:constraint_beneficiary(transition_causality__hybrid_trigger_reading, international_trade_sector).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, non_reserve_currency_nations).
narrative_ontology:constraint_victim(transition_causality__hybrid_trigger_reading, gold_window_claimants).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, triffin_dilemma).
narrative_ontology:constraint_vindicates(transition_causality__hybrid_trigger_reading, exorbitant_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the world's reserve currency under pegged gold convertibility at $35 per ounce; administers the gold window and sets macroeconomic policy without symmetric balance-of-payments constraint. Bears nominal convertibility obligation but exercises unique fiscal and monetary autonomy, capturing seigniorage from foreign dollar holdings.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, us_reserve_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__hybrid_trigger_reading, us_reserve_issuer, beneficiary).

% Must maintain dollar pegs through domestic austerity or capital controls when facing deficits; absorb imported inflation from US monetary policy; bear asymmetric adjustment burdens while lacking reserve-currency privilege or voice in US policy.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, non_reserve_currency_nations, payer,
    organized, biographical, constrained, national).

% Benefits from stable exchange rates and predictable cross-border payment infrastructure under the Bretton Woods fixed-rate regime; finances and facilitates trade expansion without bearing systemic adjustment costs.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, international_trade_sector, beneficiary,
    powerful, biographical, mobile, global).

% Accumulate dollar reserves through trade surpluses and periodically convert them to gold at the fixed $35 per ounce rate; undermine US gold reserves and signal collapsing confidence in the parity, yet bear convertibility suspension risk.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, gold_window_claimants, payer,
    powerful, biographical, mobile, national).

% Monitors exchange-rate compliance, approves devaluations, and conditionally lends to deficit nations to maintain the pegged system; enforces the rules of fixed-parity adjustment but cannot alter the reserve-center policies that drive systemic stress.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, imf_enforcement_apparatus, agenda_setter,
    institutional, generational, constrained, global).

% Diagnose the systemic contradiction between reserve-currency provision and gold convertibility; document the widening Triffin gap and forecast the instability of the gold-exchange standard.
narrative_ontology:constraint_stakeholder(transition_causality__hybrid_trigger_reading, academic_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(transition_causality__hybrid_trigger_reading, us_reserve_issuer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed-exchange-rate regime backed by dollar-gold convertibility, reducing currency risk for international trade and creating a nominally stable monetary anchor for post-war reconstruction and trade expansion.
% TRANSFER_FUNCTION: Moves seigniorage and asymmetric adjustment costs from non-reserve-currency nations to the United States as reserve issuer; moves systemic convertibility risk to dollar reserve holders while the US delays balance-of-payments adjustment.
% ABSENT_VOICES: Non-aligned and developing nations excluded from Bretton Woods design; domestic workers in adjusting nations who bore unemployment from deflationary austerity without voice in IMF conditionality; alternative monetary architects proposing commodity-basket or fully multilateral reserve systems such as the Keynes Bancor.
% DISAPPEARANCE_RATIONALE: The constraint's disappearance in 1971 triggered a global shift to floating exchange rates, ended dollar-gold convertibility, eliminated the IMF's fixed-parity enforcement role, and forced a complete reorganization of international monetary adjustment mechanisms.
% FOUNDING_PROBLEM: Post-World War II absence of a stable international monetary standard capable of financing reconstruction and trade without relapsing into the competitive devaluations and protectionism of the interwar period.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians such as Eichengreen and Bordo and contemporaneous policymakers such as White and Keynes attest the problem was real in 1944. Post-1973 academic consensus and IMF internal retrospective reviews hold that the specific gold-exchange standard mechanism became obsolete once dollar liabilities far exceeded US gold reserves and capital mobility returned; corroboration comes from outside the US Treasury beneficiary seat.
narrative_ontology:disappearance_verdict(transition_causality__hybrid_trigger_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__hybrid_trigger_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__hybrid_trigger_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__hybrid_trigger_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__hybrid_trigger_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__hybrid_trigger_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(transition_causality__hybrid_trigger_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(transition_causality__hybrid_trigger_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.20 to 0.78 over the interval as the ratio of foreign dollar claims to US gold reserves widens, transforming latent seigniorage into an acute extraction. Suppression rises from 0.42 to 0.76 as enforcement evolves from cooperative capital controls to active market intervention (London Gold Pool, swap networks, and ultimately suspension). Theater_ratio climbs from 0.10 to 0.55 as public confidence maintenance becomes increasingly performative relative to underlying monetary fundamentals. Accessibility_collapse is high (0.68) because the network externalities of reserve currency status and institutional lock-in through IMF membership made alternative monetary architectures invisible to most policymakers until collapse. Resistance (0.58) reflects French gold conversions, academic criticism, and speculative attacks, but remains sub-critical because formal exit routes were blocked by treaty architecture until 1971.
 *
 * PERSPECTIVAL GAP:
 *   From the US Treasury seat, the constraint is a public good it provisioned and managed, with seigniorage recast as necessary compensation for liquidity provision. From the non-reserve nation seat, the same structure is an enforced transfer of adjustment costs to subsidize US fiscal autonomy and war spending. The engine computes this divergence from the structural data; both coordination benefits and asymmetric costs are authored as present, producing seat-specific classifications that diverge around the tangled rope core.
 *
 * DIRECTIONALITY LOGIC:
 *   The us_reserve_issuer sits near the full beneficiary pole: it captures seigniorage, externalizes adjustment, and retains arbitrage-grade exit (suspending convertibility). Non_reserve_currency_nations sit near the full target pole: they pay through forced austerity and inflation importation with constrained exit (IMF conditionality and devaluation stigma). Gold_window_claimants sit mid-to-high target: they bear convertibility risk but possess mobile exit via gold claims, differentiating their directionality from less powerful payers. International_trade_sector sits low-to-mid beneficiary, capturing coordination surplus without direct rent extraction. Directionality is structurally derived; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-war monetary chaos and lack of liquidity â was substantially solved by the late 1950s, yet the arrangement persisted because it had become institutionally embedded and because the US captured increasing seigniorage. The R5 genealogy flags this mismatch: founding_problem_status is dead while disappearance_verdict is world_rearranges, indicating the constraint persisted beyond its functional mandate. This prevents mislabeling the late-stage system as pure coordination (rope) or pure extraction (snare); the temporal metrics show monotonically rising extraction and theater, consistent with a coordination mechanism that accumulated extractive function until contingent triggers destroyed it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trigger_contingency_vs_structure,
    'Did the Bretton Woods collapse require contingent triggers (Vietnam deficits, French gold runs) to actualize, or was the structural contradiction sufficient on its own?',
    'Counterfactual analysis of alternate fiscal and monetary histories; examine whether the Triffin gap (dollar claims versus gold stock) would have forced closure even without Vietnam War deficits or de Gaulle''s political gold conversions.',
    'If contingent triggers were necessary, the constraint is a tangled rope that failed through exogenous shock; if structural alone, it approaches an overdetermined collapse reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trigger_contingency_vs_structure, conceptual, 'Contingency versus structural sufficiency in transition causality').

omega_variable(
    coordination_extraction_separability,
    'Was the fixed-exchange-rate coordination function inherently coupled to the US asymmetric reserve-issuer extraction, or could the coordination have been maintained without the dollar''s exorbitant privilege?',
    'Historical analysis of SDR proposals and alternative reserve-asset designs (Triffin Plan, Keynes Bancor) to test whether the same coordination could have operated with symmetric adjustment obligations.',
    'If inseparable, the constraint''s extraction is structurally embedded in its coordination mechanism; if separable, the extraction was a contingent political choice layered onto coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in Bretton Woods').

omega_variable(
    us_exit_as_trigger,
    'Did the US decision to close the gold window constitute an endogenous policy choice within the constraint, or an exogenous trigger rupturing it?',
    'Examine whether US fiscal and monetary policy was structurally compelled by the Triffin Dilemma or retained autonomy; assess if the suspension was an internal regime change or external shock.',
    'If endogenous, the collapse was generated by the constraint''s own administrator; if exogenous, the constraint was ruptured by its beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_exit_as_trigger, conceptual, 'Endogeneity of the Nixon Shock relative to the constraint structure').

omega_variable(
    kernel_reading_decomposition,
    'This constraint instantiates the hybrid_trigger reading of the transition_causality kernel; how would classification change if the contingent_choice or overdetermined_collapse readings were adopted instead?',
    'Cross-reading comparison: the contingent_choice reading would emphasize avoidable policy error and shift blame toward US Treasury discretion; the overdetermined_collapse reading would treat the constraint as a degraded structure destined to fail regardless of maintenance.',
    'Adopting a sibling reading would relocate the causal locus from structural-then-trigger to pure agency or pure structure, altering the directionality assigned to the US Treasury seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Kernel decomposition and sibling reading structural deltas').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__hybrid_trigger_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tran_tr_t0, transition_causality__hybrid_trigger_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tran_tr_t6, transition_causality__hybrid_trigger_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(tran_tr_t12, transition_causality__hybrid_trigger_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(tran_tr_t18, transition_causality__hybrid_trigger_reading, theater_ratio, 18, 0.35).
narrative_ontology:measurement(tran_tr_t24, transition_causality__hybrid_trigger_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(tran_tr_t27, transition_causality__hybrid_trigger_reading, theater_ratio, 27, 0.55).

% Extraction over time
narrative_ontology:measurement(tran_be_t0, transition_causality__hybrid_trigger_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tran_be_t6, transition_causality__hybrid_trigger_reading, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(tran_be_t12, transition_causality__hybrid_trigger_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(tran_be_t18, transition_causality__hybrid_trigger_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(tran_be_t24, transition_causality__hybrid_trigger_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(tran_be_t27, transition_causality__hybrid_trigger_reading, base_extractiveness, 27, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tran_su_t0, transition_causality__hybrid_trigger_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(tran_su_t6, transition_causality__hybrid_trigger_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(tran_su_t12, transition_causality__hybrid_trigger_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(tran_su_t18, transition_causality__hybrid_trigger_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement(tran_su_t24, transition_causality__hybrid_trigger_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(tran_su_t27, transition_causality__hybrid_trigger_reading, suppression_requirement, 27, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__hybrid_trigger_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, contingent_choice_reading).
narrative_ontology:affects_constraint(transition_causality__hybrid_trigger_reading, overdetermined_collapse_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the transition_causality constraint family, decomposed per the epsilon-invariance principle. The three readings (hybrid_trigger, contingent_choice, overdetermined_collapse) are structurally distinct claims about the same historical transition; they are linked as siblings, not as causal dependencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
