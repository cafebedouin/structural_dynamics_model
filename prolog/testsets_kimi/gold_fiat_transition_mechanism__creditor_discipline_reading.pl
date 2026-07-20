% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__creditor_discipline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__creditor_discipline_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gold_fiat_transition_mechanism__creditor_discipline_reading
 *   human_readable: Gold-to-Fiat Transition: Creditor Discipline Elimination
 *   domain: monetary_economics/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the creditor-discipline reading of the
 *   gold-to-fiat transition kernel. It treats the post-1971 monetary
 *   arrangement not as a neutral technical upgrade but as the deliberate
 *   elimination of creditor nations' gold-redemption veto, effecting a
 *   geopolitical power shift toward debtor nations and especially the
 *   reserve-currency issuer. The constraint binds creditor nations (who lost
 *   enforcement leverage) and non-reserve holders (who face tightened dollar
 *   dependency), while the reserve issuer escapes balance-of-payments
 *   discipline. The claim/metric independence is maintained: the claimed type
 *   is tangled_rope because the arrangement still coordinates global
 *   liquidity, but the metrics describe a high-extraction regime where that
 *   coordination function is fused with asymmetric extraction.
 *
 * KEY AGENTS:
 *   - reserve_currency_issuer (US): Primary beneficiary and agenda-setter â institutional power, arbitrage-grade exit from BoP discipline.
 *   - creditor_nations (surplus accumulators): Primary target â powerful but constrained exit, lost redemption leverage.
 *   - non_reserve_debtor_nations: Secondary target â powerless, trapped exit, must acquire dollars and face IMF conditionality.
 *   - international_monetary_institutions: Agenda-setter â institutional power, analytical exit, administers and legitimizes the regime.
 *   - gold_standard_advocates: Excluded voice â constrained exit, structurally absent from policy forums.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.8).
domain_priors:suppression_score(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.68).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__creditor_discipline_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__creditor_discipline_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__creditor_discipline_reading, "Gold-to-Fiat Transition: Creditor Discipline Elimination").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__creditor_discipline_reading, "monetary_economics/political_economy").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__creditor_discipline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__creditor_discipline_reading, '11f9a268-eeb6-454a-adc2-0bff1a6b172d').
narrative_ontology:cs_kernel_codification('11f9a268-eeb6-454a-adc2-0bff1a6b172d', formalized).
narrative_ontology:cs_authority_grounding('11f9a268-eeb6-454a-adc2-0bff1a6b172d', extraction).
narrative_ontology:cs_interpretation_layer_present('11f9a268-eeb6-454a-adc2-0bff1a6b172d').
narrative_ontology:cs_reading_relation('11f9a268-eeb6-454a-adc2-0bff1a6b172d', gold_fiat_transition_mechanism__automatic_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('11f9a268-eeb6-454a-adc2-0bff1a6b172d', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('11f9a268-eeb6-454a-adc2-0bff1a6b172d', foundational, creditor_enforced_settlement_necessary_for_stability).
narrative_ontology:cs_axiom_status(creditor_enforced_settlement_necessary_for_stability, holdable).
narrative_ontology:cs_axiom_grounding('11f9a268-eeb6-454a-adc2-0bff1a6b172d', creditor_enforced_settlement_necessary_for_stability, empirically_contingent).
narrative_ontology:cs_reference_frame('11f9a268-eeb6-454a-adc2-0bff1a6b172d', gold_anchor_monetary_order).
narrative_ontology:cs_drift_state('11f9a268-eeb6-454a-adc2-0bff1a6b172d', post_nixon_shock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11f9a268-eeb6-454a-adc2-0bff1a6b172d', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the global reserve currency; can settle external obligations in domestically printed money; immune from traditional balance-of-payments crises that force austerity on others; sets the agenda through IMF quota dominance and swap-line architecture.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer, beneficiary).

% Gain fiscal flexibility under the post-gold regime; can run persistent deficits without facing immediate redemption-driven austerity; benefit from elastic global liquidity, though non-reserve issuers still face dollar-acquisition constraints.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, debtor_nations, beneficiary,
    moderate, generational, constrained, national).

% Accumulate surplus reserves in fiat form; lost the ability to demand gold redemption to enforce balance-of-payments discipline on debtor nations; exposed to inflation and depreciation of reserve assets; structural leverage converted into credit risk.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, creditor_nations, payer,
    powerful, generational, constrained, global).

% Must acquire reserve currency to service sovereign debt and finance trade; subject to IMF conditionality during crises; face tightened external constraint compared to the gold era because they cannot print settlement currency and creditor nations can no longer force debtor discipline via redemption.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, non_reserve_debtor_nations, payer,
    powerless, biographical, trapped, national).

% Administer balance-of-payments support and surveillance; enforce conditionality; legitimize the fiat reserve regime as the necessary global monetary order; derive institutional authority from managing the post-gold architecture.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, international_monetary_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Argue for restoration of commodity-backed settlement and creditor-enforced discipline; structurally excluded from mainstream macroeconomic policy forums and treated as anachronistic.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__creditor_discipline_reading, gold_standard_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__creditor_discipline_reading, reserve_currency_issuer).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__creditor_discipline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides elastic international liquidity and trade settlement without the deflationary rigidity of gold convertibility, enabling persistent current-account imbalances to be financed rather than forcibly corrected.
% TRANSFER_FUNCTION: Moves balance-of-payments discipline costs and inflation risk from debtor nations (especially the reserve issuer) to creditor nations and non-reserve holders, by eliminating gold redemption as a creditor veto mechanism and replacing it with fiat reserve accumulation.
% ABSENT_VOICES: Gold-standard advocates and creditor-nation publics who bear the inflation tax on accumulated reserves; non-reserve debtor populations subjected to IMF austerity without the historical counterweight of creditor redemption demands; future generations holding devalued sovereign debt instruments.
% DISAPPEARANCE_RATIONALE: If the fiat reserve system and its elimination of creditor gold redemption vanished overnight, creditor nations would regain structural leverage to enforce balance-of-payments discipline; the reserve issuer would lose seigniorage and fiscal autonomy; global trade and debt structures would reorganize around settlement-discipline or a new anchor.
% FOUNDING_PROBLEM: The interwar gold standard and Bretton Woods system imposed deflationary liquidity constraints and periodic redemption crises that forced procyclical austerity, culminating in the Triffin dilemma and the Nixon Shock.
% FOUNDING_PROBLEM_CORROBORATION: Keynesian and post-war Atlantic planners attested the liquidity constraint as live. Creditor-nation economists and gold-standard advocates contest that the problem warranted eliminating creditor settlement rights; independent monetary historians outside the reserve-issuer beneficiary camp document that the 'problem' was politically manufactured to enable deficit financing.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__creditor_discipline_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__creditor_discipline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__creditor_discipline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__creditor_discipline_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__creditor_discipline_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__creditor_discipline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80) because the elimination of gold redemption transfers real purchasing power and policy autonomy from creditors to the reserve issuer via inflation risk and seigniorage. Suppression (0.68) reflects the active enforcement required to prevent reversion to gold or alternative reserve systems, including IMF conditionality and dollar-denominated sanctions architecture. Theater ratio (0.52) captures the widening gap between the regime's presentation as a global public good and its operation as an extraction mechanism. Accessibility collapse (0.72) reflects the near-impossibility of exiting the dollar system for trade and reserves once integrated. Resistance (0.58) reflects ongoing de-dollarization efforts and creditor-nation complaints.
 *
 * PERSPECTIVAL GAP:
 *   From the reserve-issuer seat, the arrangement is necessary global infrastructure (rope-like); from the creditor-nation seat, it is the loss of legitimate enforcement rights (snare-like); from the non-reserve debtor seat, it is a tightened external constraint with no creditor counterweight. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Reserve_currency_issuer has arbitrage-grade exit (can print settlement medium) and is a declared beneficiary, yielding very low directionality. Debtor_nations have constrained exit but are net beneficiaries of fiscal flexibility, yielding low-moderate directionality. Creditor_nations have constrained exit and are declared victims, yielding high directionality. Non_reserve_debtor_nations have trapped exit and are victims, yielding very high directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (gold's deflationary rigidity) was plausibly live in 1945-1971, but the constraint's persistence after the Cold War and through multiple dollar cycles suggests the coordination function has atrophied into inertial extraction. The R5 genealogy (dead founding problem + world_rearranges disappearance) signals mandatrophy risk: the arrangement persists because the reserve issuer and institutions benefit, not because gold liquidity scarcity remains unsolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_issuer_exemption_permanence,
    'Is the reserve issuer''s exemption from balance-of-payments discipline a permanent structural feature of the fiat regime, or contingent on geopolitical and dollar-confidence conditions that could reverse?',
    'Historical analysis of reserve-currency transitions (sterling-to-dollar) and empirical tracking of dollar-share in reserves and settlement.',
    'If contingent, the constraint''s extractiveness may be self-limiting via confidence crises; if permanent, extraction is structurally embedded for the regime''s lifespan.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_issuer_exemption_permanence, empirical, 'Whether reserve-issuer privilege is structural or contingent.').

omega_variable(
    creditor_veto_counterfactual,
    'Would restored creditor veto via gold or SDR redemption actually enforce discipline, or would it simply trigger sovereign default cascades?',
    'Counterfactual simulation and historical comparison of gold-standard adjustment mechanisms versus modern sovereign-debt workouts.',
    'If veto restoration would enforce discipline without systemic collapse, the current regime''s elimination of veto appears more extractive; if it would trigger cascades, the coordination function of fiat flexibility is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creditor_veto_counterfactual, empirical, 'Whether creditor veto restoration is viable or destructive.').

omega_variable(
    non_reserve_victim_status,
    'Does this reading correctly classify non-reserve debtor nations as victims of tightened constraint, or are they net beneficiaries of the liquidity elasticity?',
    'Cross-national panel analysis of growth and crisis incidence under gold versus fiat regimes, disaggregated by reserve-currency status.',
    'If non-reserve debtors are net beneficiaries, the victim set shrinks to creditor nations alone and the constraint edges toward snare; if they are victims, the extraction is broader and more tightly tangled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_reserve_victim_status, conceptual, 'Ambiguity in classifying non-reserve debtors under this reading.').

omega_variable(
    committer_reading_separation,
    'Does the creditor-discipline reading''s high-epsilon classification depend on treating the elimination of creditor veto as the primary structural delta, or would it collapse into the automatic-constraint reading if the power-shift framing were removed?',
    'Comparative structural analysis of the three kernel readings to test whether epsilon remains high under the automatic reading''s neutral framing.',
    'If epsilon collapses under neutral framing, the extraction is in the reading''s lens rather than the kernel; if epsilon stays high, the kernel itself is extractive regardless of framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_separation, conceptual, 'Whether high epsilon is reading-dependent or kernel-inherent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__creditor_discipline_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gftm_cdr_tr_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gftm_cdr_tr_t10, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(gftm_cdr_tr_t20, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(gftm_cdr_tr_t30, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(gftm_cdr_tr_t40, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 40, 0.47).
narrative_ontology:measurement(gftm_cdr_tr_t50, gold_fiat_transition_mechanism__creditor_discipline_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(gftm_cdr_be_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(gftm_cdr_be_t10, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(gftm_cdr_be_t20, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(gftm_cdr_be_t30, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(gftm_cdr_be_t40, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(gftm_cdr_be_t50, gold_fiat_transition_mechanism__creditor_discipline_reading, base_extractiveness, 50, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(gftm_cdr_su_t0, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gftm_cdr_su_t10, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(gftm_cdr_su_t20, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(gftm_cdr_su_t30, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(gftm_cdr_su_t40, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(gftm_cdr_su_t50, gold_fiat_transition_mechanism__creditor_discipline_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__creditor_discipline_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
