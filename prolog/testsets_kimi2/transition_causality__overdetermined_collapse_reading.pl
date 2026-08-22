% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: transition_causality__overdetermined_collapse_reading
 *   human_readable: Bretton Woods Fixed Exchange Rate Regime â Overdetermined Collapse Reading
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story models the Bretton Woods fixed exchange rate regime
 *   through the overdetermined collapse reading of the transition_causality
 *   kernel. The reading treats the Triffin Dilemma â the impossibility of
 *   simultaneously maintaining dollar-gold convertibility and supplying
 *   global liquidity â as a mountain-like structural necessity that made
 *   the regime's collapse inevitable. All participant actors are cast as
 *   structurally constrained by this logical contradiction. Sibling readings
 *   contest this naturalization: the contingent_choice_reading locates
 *   causality in avoidable policy decisions, while the hybrid_trigger_reading
 *   sees structural contradictions as necessary but insufficient without
 *   contingent events. The story authors a mountain claim while recording
 *   regime-level metrics that describe the constructed arrangement's actual
 *   operation; the divergence is intentional and diagnostic.
 *
 * KEY AGENTS:
 *   - us_monetary_authorities: Primary payer (institutional/trapped) â bear the liquidity-confidence contradiction directly.
 *   - foreign_central_banks: Payer (organized/trapped) â locked into dollar reserve accumulation and parity defense.
 *   - dollar_reserve_holders: Payer (organized/trapped) â exposed to convertibility suspension and purchasing-power erosion.
 *   - participant_governments: Payer (organized/trapped) â subject to asymmetric adjustment burdens and IMF conditionality.
 *   - structural_economists: Analytical observer (analytical/analytical) â models the overdetermined collapse without bearing its costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.42).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.58).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Fixed Exchange Rate Regime â Overdetermined Collapse Reading").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, 'e1a0f94a-2687-4134-9999-1fd1a31265cf').
narrative_ontology:cs_kernel_codification('e1a0f94a-2687-4134-9999-1fd1a31265cf', formalized).
narrative_ontology:cs_authority_grounding('e1a0f94a-2687-4134-9999-1fd1a31265cf', lineage).
narrative_ontology:cs_interpretation_layer_present('e1a0f94a-2687-4134-9999-1fd1a31265cf').
narrative_ontology:cs_reading_relation('e1a0f94a-2687-4134-9999-1fd1a31265cf', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('e1a0f94a-2687-4134-9999-1fd1a31265cf', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('e1a0f94a-2687-4134-9999-1fd1a31265cf', foundational, structural_inevitability_of_regime_collapse).
narrative_ontology:cs_axiom_status(structural_inevitability_of_regime_collapse, holdable).
narrative_ontology:cs_axiom_grounding('e1a0f94a-2687-4134-9999-1fd1a31265cf', structural_inevitability_of_regime_collapse, empirically_contingent).
narrative_ontology:cs_axiom('e1a0f94a-2687-4134-9999-1fd1a31265cf', foundational, triffin_dilemma_as_binding_logical_limit).
narrative_ontology:cs_axiom_status(triffin_dilemma_as_binding_logical_limit, holdable).
narrative_ontology:cs_axiom_grounding('e1a0f94a-2687-4134-9999-1fd1a31265cf', triffin_dilemma_as_binding_logical_limit, empirically_contingent).
narrative_ontology:cs_reference_frame('e1a0f94a-2687-4134-9999-1fd1a31265cf', dollar_gold_paradigm).
narrative_ontology:cs_drift_state('e1a0f94a-2687-4134-9999-1fd1a31265cf', post_nixon_shock_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e1a0f94a-2687-4134-9999-1fd1a31265cf', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, us_monetary_authorities).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, foreign_central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, dollar_reserve_holders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, participant_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for maintaining dollar-gold convertibility while supplying global liquidity; the Triffin arithmetic traps them between domestic monetary objectives and international confidence requirements. Defending the peg required ever-larger swap lines and gold-market interventions that deepened the underlying contradiction.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_monetary_authorities, payer,
    institutional, generational, trapped, global).

% Accumulate dollar reserves to maintain parity commitments; bear devaluation and inflation risk from US deficit spending. Diversifying into gold or other currencies would break the parity structure and trigger a run, so they are structurally locked into supporting the dollar.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, foreign_central_banks, payer,
    organized, biographical, trapped, global).

% Hold dollar-denominated assets as the reserve base of the system; exposed to convertibility suspension and purchasing-power erosion. No alternative reserve asset exists at the scale required, making exit from dollar exposure impossible without systemic collapse.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, dollar_reserve_holders, payer,
    organized, biographical, trapped, global).

% Maintain fixed parities through austerity, capital controls, and IMF stabilization programs. Adjustment burdens fall asymmetrically on deficit countries, while surplus countries face pressure to recycle dollars, locking all into the same structural contradiction.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, participant_governments, payer,
    organized, biographical, trapped, global).

% Model the Triffin Dilemma and the overdetermined collapse trajectory; they observe the contradiction between liquidity provision and convertibility but do not administer the regime or bear its costs directly.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, structural_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable nominal anchor for post-war exchange rates, reducing transaction costs and currency risk in international trade and investment by fixing parities against the dollar and gold.
% TRANSFER_FUNCTION: Moves liquidity and adjustment costs from the reserve-center country to peripheral economies; transfers seigniorage, inflation risk, and convertibility exposure to dollar-reserve holders and deficit-country governments.
% ABSENT_VOICES: Proponents of commodity-backed alternatives such as the bancor and early advocates of generalized floating were structurally excluded from the original Bretton Woods design; surplus countries demanding symmetric adjustment were marginalized in IMF governance.
% DISAPPEARANCE_RATIONALE: The regime's disappearance forced a global shift to floating exchange rates, terminated gold-dollar convertibility, eliminated the formal parity structure, and triggered a decades-long search for alternative reserve assets and monetary anchors.
% FOUNDING_PROBLEM: Chaotic interwar competitive devaluations, currency warfare, and post-war monetary instability that threatened trade reconstruction and investment recovery.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians corroborate the interwar monetary chaos as the founding problem. Structuralist and dependency theorists outside the reserve-center consensus corroborate that the solution outlived its function and became a source of systemic extraction; no corroboration from within the benefiting parties is required because the reading identifies no concentrated beneficiary set.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__overdetermined_collapse_reading_tests).

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
 *   Extraction (0.42) reflects asymmetric adjustment burdens, seigniorage transfers, and dollar-overhang costs that intensified as the regime matured. Suppression (0.58) reflects capital controls, IMF conditionality, and the active exclusion of alternative reserve proposals needed to sustain the parity structure. Theater (0.38) reflects confidence-maintenance operations (London gold pool, swap networks, rhetorical defense of convertibility) that performed stability while underlying arithmetic deteriorated. Accessibility collapse (0.82) reflects the absence of a scalable alternative reserve asset once the regime was entrenched. Resistance (0.28) reflects intermittent challenges (French gold conversions, sterling devaluations) that were ultimately insufficient against the structural logic. The claim is mountain because the reading asserts the Triffin Dilemma is an irreducible logical limit; the metrics independently describe a heavily enforced, asymmetrically burdensome constructed regime.
 *
 * PERSPECTIVAL GAP:
 *   The US monetary authorities experienced the constraint as a policy problem to be managed; from their perspective, the regime was a coordination device they administered. Foreign central banks and peripheral governments experienced the same structure as an externally imposed burden with asymmetric adjustment costs. The overdetermined reading collapses this gap by asserting both seats were equally trapped by the same mountain; the metrics preserve the divergence by showing high extraction and suppression that fell asymmetrically on non-center actors.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared agents are in the victim/payer set because the overdetermined reading asserts universal constraint by the Triffin logic. There are no declared beneficiaries: the US seigniorage advantage is treated as epiphenomenal and ultimately unsustainable within the same trap. The engine will compute high directionality (near 1.0) for all payer seats, amplifying effective extraction. No directionality overrides are needed because the structural derivation matches the reading's universal-victim framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the Bretton Woods regime as pure coordination (Rope) by requiring victim declarations and showing active enforcement. It also prevents mislabeling it as pure extraction (Snare) by allowing the mountain claim to be tested against metrics: a genuine mountain would show negligible extraction, suppression, and theater. The authored metrics show significant values in all three, inviting false-summit analysis. The founding problem (interwar monetary chaos) is dead, and the regime persisted beyond its functional life, but the reading explains the persistence as structural inertia rather than capture â mandatrophy resolution depends on whether the persistence benefited a capturer (gain_flow is absent) or was pure inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_naturalization_ambiguity,
    'Does treating the Triffin Dilemma as a mountain naturalize a historically specific reserve-currency arrangement, obscuring its constructed distributional features?',
    'Historical comparative analysis of alternative reserve regimes (bancor, multi-polar SDR systems) to test whether the dilemma persists across institutional forms or is specific to the dollar-gold nexus.',
    'If the dilemma is institution-specific, the mountain claim collapses and the constraint reclassifies as tangled rope or snare; if it persists across all fixed-rate reserve-currency arrangements, the mountain claim is structurally supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_naturalization_ambiguity, conceptual, 'Natural-law versus constructed-institution ambiguity for the Triffin Dilemma').

omega_variable(
    counterfactual_viability_of_alternatives,
    'Would an alternative reserve-asset regime have averted the collapse, or were the contradictions deeper than the institutional form?',
    'Counterfactual economic modeling and historical analysis of the Keynes Plan and subsequent SDR proposals to assess whether the contradictions were genuinely overdetermined.',
    'If viable alternatives existed, the inevitability claim weakens and the constraint shifts toward contingent choice or hybrid trigger readings; if no alternative could escape the liquidity-confidence bind, the overdetermined reading is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_viability_of_alternatives, empirical, 'Empirical test of counterfactual regime viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tc_ocr_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tc_ocr_tr_t5, transition_causality__overdetermined_collapse_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(tc_ocr_tr_t10, transition_causality__overdetermined_collapse_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(tc_ocr_tr_t15, transition_causality__overdetermined_collapse_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(tc_ocr_tr_t20, transition_causality__overdetermined_collapse_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(tc_ocr_tr_t25, transition_causality__overdetermined_collapse_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(tc_ocr_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tc_ocr_be_t5, transition_causality__overdetermined_collapse_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(tc_ocr_be_t10, transition_causality__overdetermined_collapse_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(tc_ocr_be_t15, transition_causality__overdetermined_collapse_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(tc_ocr_be_t20, transition_causality__overdetermined_collapse_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(tc_ocr_be_t25, transition_causality__overdetermined_collapse_reading, base_extractiveness, 25, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(tc_ocr_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(tc_ocr_su_t5, transition_causality__overdetermined_collapse_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(tc_ocr_su_t10, transition_causality__overdetermined_collapse_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(tc_ocr_su_t15, transition_causality__overdetermined_collapse_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(tc_ocr_su_t20, transition_causality__overdetermined_collapse_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(tc_ocr_su_t25, transition_causality__overdetermined_collapse_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
