% ============================================================================
% CONSTRAINT STORY: transition_causality__overdetermined_collapse_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Bretton Woods Fixed-Rate Regime Structural Contradictions (Overdetermined Collapse Reading)
 *   domain: economic/political/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the overdetermined_collapse_reading of
 *   the transition_causality kernel: the Bretton Woods fixed exchange rate
 *   regime collapsed because multiple reinforcing structural contradictions
 *   (Triffin dilemma, asymmetric adjustment, n-th currency problem, growing
 *   eurodollar markets, US fiscal-monetary expansion) made its failure
 *   inevitable regardless of policy choices. The Triffin dilemma operates as
 *   a mountain constraint — a structural feature of any reserve-currency
 *   system — not a contingent policy failure. All actors constrained by the
 *   fixed-rate regime (central banks, governments, traders, citizens, even
 *   the reserve issuer) were victims of the structural logic; no party
 *   collected net rents sufficient to maintain the system. The claimed_type
 *   is mountain (the reading's structural claim) while the authored metrics
 *   describe a regime that extracted policy autonomy and imposed adjustment
 *   costs — the engine measures this divergence; do not reconcile.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__overdetermined_collapse_reading, 0.65).
domain_priors:suppression_score(transition_causality__overdetermined_collapse_reading, 0.55).
domain_priors:theater_ratio(transition_causality__overdetermined_collapse_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(transition_causality__overdetermined_collapse_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__overdetermined_collapse_reading, mountain).
narrative_ontology:human_readable(transition_causality__overdetermined_collapse_reading, "Bretton Woods Fixed-Rate Regime Structural Contradictions (Overdetermined Collapse Reading)").
narrative_ontology:topic_domain(transition_causality__overdetermined_collapse_reading, "economic/political/international_finance").

domain_priors:emerges_naturally(transition_causality__overdetermined_collapse_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__overdetermined_collapse_reading, '7d11a744-b897-4e3c-8048-1c9f34e16a1e').
narrative_ontology:cs_kernel_codification('7d11a744-b897-4e3c-8048-1c9f34e16a1e', distributed).
narrative_ontology:cs_authority_grounding('7d11a744-b897-4e3c-8048-1c9f34e16a1e', distributed).
narrative_ontology:cs_reading_relation('7d11a744-b897-4e3c-8048-1c9f34e16a1e', transition_causality__contingent_choice_reading, forecloses).
narrative_ontology:cs_reading_relation('7d11a744-b897-4e3c-8048-1c9f34e16a1e', transition_causality__hybrid_trigger_reading, forecloses).
narrative_ontology:cs_axiom('7d11a744-b897-4e3c-8048-1c9f34e16a1e', foundational, collapse_was_overdetermined_by_structural_contradictions).
narrative_ontology:cs_axiom_status(collapse_was_overdetermined_by_structural_contradictions, holdable).
narrative_ontology:cs_axiom_grounding('7d11a744-b897-4e3c-8048-1c9f34e16a1e', collapse_was_overdetermined_by_structural_contradictions, empirically_contingent).
narrative_ontology:cs_axiom('7d11a744-b897-4e3c-8048-1c9f34e16a1e', foundational, triffin_dilemma_is_mountain_constraint).
narrative_ontology:cs_axiom_status(triffin_dilemma_is_mountain_constraint, holdable).
narrative_ontology:cs_axiom_grounding('7d11a744-b897-4e3c-8048-1c9f34e16a1e', triffin_dilemma_is_mountain_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('7d11a744-b897-4e3c-8048-1c9f34e16a1e', structural_inevitability_framework).
narrative_ontology:cs_drift_state('7d11a744-b897-4e3c-8048-1c9f34e16a1e', post_1971_nixon_shock, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('7d11a744-b897-4e3c-8048-1c9f34e16a1e', '').
narrative_ontology:cs_kernel_id(transition_causality__overdetermined_collapse_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, central_banks).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, governments).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, international_traders).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, citizens_under_fixed_rates).
narrative_ontology:constraint_victim(transition_causality__overdetermined_collapse_reading, us_reserve_issuer).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(transition_causality__overdetermined_collapse_reading, us_reserve_issuer).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, triffin_dilemma_as_structural_law).
narrative_ontology:constraint_vindicates(transition_causality__overdetermined_collapse_reading, fixed_rate_regime_inevitability_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Forced to hold large dollar reserves and intervene in currency markets to maintain pegs; lost domestic monetary autonomy; bore the cost of defending parities against speculative pressure. Exit meant abandoning the international monetary system entirely.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, central_banks, payer,
    powerful, biographical, constrained, national).

% Sacrificed fiscal and monetary policy autonomy to maintain fixed parities; faced IMF conditionality when reserves ran low; political legitimacy tied to exchange rate stability. Could not exit without triggering capital flight and domestic crisis.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, governments, payer,
    institutional, biographical, constrained, national).

% Benefited from stable exchange rates for trade planning but bore adjustment costs when parities misaligned; faced uncertainty during recurrent crises. Could partially hedge or shift invoicing currencies but remained embedded in the dollar system.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, international_traders, payer,
    organized, biographical, mobile, global).

% Bore the domestic costs of balance-of-payments adjustment — austerity, unemployment, inflation — with no voice in international monetary governance. Exit from the constraint was individually impossible.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, citizens_under_fixed_rates, payer,
    powerless, biographical, trapped, national).

% Enjoyed exorbitant privilege (seigniorage, lower borrowing costs) but faced the Triffin dilemma structurally: supplying global liquidity required running deficits that undermined gold convertibility. Could not exit the reserve role without collapsing the system it anchored.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, us_reserve_issuer, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(transition_causality__overdetermined_collapse_reading, us_reserve_issuer, beneficiary).

% Administered the adjustable-peg system, provided conditional lending, and surveilled parity compliance. Its authority derived from the regime's existence; it managed the adjustment process but could not alter the structural contradictions.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, imf, agenda_setter,
    institutional, generational, analytical, global).

% Diagnosed the Triffin dilemma from the 1960s onward (Triffin, Kindleberger, Mundell); debated reform vs. collapse. Their analyses shaped policy discourse but did not control the regime's trajectory.
narrative_ontology:constraint_stakeholder(transition_causality__overdetermined_collapse_reading, academic_economists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The fixed exchange rate regime coordinated international trade and capital flows by providing stable, predictable exchange rates, solving the coordination problem of currency volatility that had plagued the interwar period.
% TRANSFER_FUNCTION: The regime transferred policy autonomy and seigniorage from national authorities to the system's structural logic (the Triffin dilemma), and transferred adjustment costs asymmetrically onto deficit countries and reserve-holding nations while the reserve issuer gained seigniorage but faced structural insolvency risk.
% ABSENT_VOICES: Newly independent post-colonial nations and non-aligned movement countries were excluded from the Bretton Woods design conference but bore the system's constraints; they would have objected to the asymmetric adjustment burden and the dollar's privileged position. Their voices entered only later through the Group of 77 and UNCTAD demands for a New International Economic Order.
% DISAPPEARANCE_RATIONALE: When the fixed-rate constraint collapsed in 1971–73, the world monetary system fundamentally rearranged: floating rates became the norm, monetary policy regained domestic autonomy, financial markets globalized, and the dollar remained dominant but unanchored — a new structural equilibrium emerged.
% FOUNDING_PROBLEM: Post-war monetary chaos, competitive devaluations, and trade collapse required a stable international monetary framework for reconstruction and the revival of multilateral trade.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians (Eichengreen 'Globalizing Capital', Bordo 'Bretton Woods System') and IMF archives document the founding problem; the system's persistence two decades after European reconstruction was complete is attested by Triffin's own testimony to Congress (1960) and G10 deliberations on reform (1960s) — sources outside the US beneficiary position.
narrative_ontology:disappearance_verdict(transition_causality__overdetermined_collapse_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__overdetermined_collapse_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__overdetermined_collapse_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__overdetermined_collapse_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__overdetermined_collapse_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.65) reflects the regime's extraction of monetary autonomy and imposition of asymmetric adjustment costs. Suppression (0.55) captures capital controls, IMF conditionality, and the gold pool's market interventions — active enforcement that intensified over time. Theater ratio (0.25) is low: the coordination function (stable trade invoicing) was genuine until structural contradictions overwhelmed it. Accessibility collapse (0.85) is high: once the Triffin dilemma was understood (early 1960s), alternatives (floating, Bancor, SDRs) were either blocked or insufficient. Resistance (0.15) is low: no coalition could overcome the structural logic; European central banks accommodated, the US delayed, the IMF managed decline.
 *
 * PERSPECTIVAL GAP:
 *   The overdetermined collapse reading claims the constraint was a mountain (inevitable structural logic) — from the analytical observer seat this computes as mountain. From the payer seats (central banks, governments) the lived experience was of an extractive, enforced regime — the engine's per-seat computation should reveal this divergence: the same structure appears as mountain from the analytical seat and as snare/tangled_rope from the payer seats. This seat divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   All named stakeholders are payers — the reading posits no net beneficiaries. The US reserve issuer has secondary_role beneficiary (exorbitant privilege) but is primarily a payer (Triffin burden). Central banks and governments are powerful payers constrained by the system they administer. Citizens are powerless trapped payers. The IMF is the agenda_setter but does not extract; it administers a structure that extracts from everyone. Academic economists are analytical observers. Directionality for all payer seats is high (d near 1.0); the engine will compute high effective extraction for all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-war monetary stability for reconstruction) was solved by the mid-1950s, but the regime persisted for two more decades — a classic mandatrophy case. The overdetermined reading resolves this by claiming the constraint was never a policy choice but a structural inevitability: mandatrophy is misdiagnosed because there was no 'mandate' to outlive, only a mountain to hit. The contingent_choice_reading would call this mandatrophy; this reading forecloses that diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    triffin_natural_law_vs_contingent_design,
    'Is the Triffin Dilemma a genuine natural law of any reserve-currency system (mountain), or a contingent feature of the specific Bretton Woods design (gold peg + dollar centrality + current account convertibility)?',
    'Comparative analysis of other reserve-currency systems (sterling pre-1914, post-1973 dollar, hypothetical SDR system) — if all exhibit the same structural contradiction, it is mountain; if only Bretton Woods does, it is contingent design.',
    'If mountain, the overdetermined reading is vindicated and the contingent_choice_reading is foreclosed; if contingent, the hybrid_trigger_reading gains ground and policy choices matter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(triffin_natural_law_vs_contingent_design, empirical, 'Whether the Triffin dilemma is a universal structural law or a Bretton Woods-specific artifact.').

omega_variable(
    overdetermination_vs_contingent_policy,
    'Would the system have collapsed in the early 1970s without the specific US policy choices of 1965–71 (Vietnam War spending, Great Society, refusal to raise taxes, monetary accommodation)?',
    'Counterfactual simulation: hold US fiscal-monetary policy at 1965 levels, model gold drain and confidence dynamics. If collapse still occurs ~1970s, overdetermination holds; if system survives indefinitely, contingent_choice_reading gains.',
    'Determines whether the collapse was truly overdetermined (multiple sufficient causes) or whether a single necessary cause (US policy) dominated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermination_vs_contingent_policy, conceptual, 'Whether US policy choices were necessary for the timing and mode of collapse.').

omega_variable(
    us_net_beneficiary_or_victim,
    'Did the US as reserve issuer benefit net from the system (exorbitant privilege > Triffin burden) or was it also a structural victim?',
    'Net present value calculation of seigniorage, lower borrowing costs, and policy flexibility vs. cost of defending gold parity, eventual loss of monetary autonomy, and post-1971 inflation — assessed by independent economic historians.',
    'If US was net beneficiary, the victim structure is asymmetric (Triffin as extraction mechanism); if net victim, the victim structure is symmetric (all constrained), strengthening the mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_net_beneficiary_or_victim, empirical, 'Whether the reserve issuer''s position was structurally beneficial or burdensome on net.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of alternatives (capital controls, IMF conditionality, gold pool) structural (legal/institutional barriers) or internalized (policy elites'' belief in fixed rates as necessary)?',
    'Post-1973 suppression trajectory: if capital controls were rapidly lifted and policy elites embraced floating rates, suppression was largely structural; if ideological resistance to floating persisted, internalized component was significant.',
    'If internalized suppression was large, the constraint''s effective suppression exceeds the structural measure — the mountain claim (natural law) is reinforced by cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the fixed-rate regime''s final decade.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__overdetermined_collapse_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_tr_t0, transition_causality__overdetermined_collapse_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_tr_t6, transition_causality__overdetermined_collapse_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_tr_t12, transition_causality__overdetermined_collapse_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_tr_t18, transition_causality__overdetermined_collapse_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_tr_t24, transition_causality__overdetermined_collapse_reading, theater_ratio, 24, 0.23).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_tr_t27, transition_causality__overdetermined_collapse_reading, theater_ratio, 27, 0.25).

% Extraction over time
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_be_t0, transition_causality__overdetermined_collapse_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_be_t6, transition_causality__overdetermined_collapse_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_be_t12, transition_causality__overdetermined_collapse_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_be_t18, transition_causality__overdetermined_collapse_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_be_t24, transition_causality__overdetermined_collapse_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_be_t27, transition_causality__overdetermined_collapse_reading, base_extractiveness, 27, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_su_t0, transition_causality__overdetermined_collapse_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_su_t6, transition_causality__overdetermined_collapse_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_su_t12, transition_causality__overdetermined_collapse_reading, suppression_requirement, 12, 0.45).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_su_t18, transition_causality__overdetermined_collapse_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_su_t24, transition_causality__overdetermined_collapse_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(transition_causality__overdetermined_collapse_reading_su_t27, transition_causality__overdetermined_collapse_reading, suppression_requirement, 27, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__overdetermined_collapse_reading, global_infrastructure).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, triffin_dilemma_as_mountain).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, bretton_woods_adjustment_mechanism).
narrative_ontology:affects_constraint(transition_causality__overdetermined_collapse_reading, eurodollar_market_emergence).

% DUAL FORMULATION NOTE:
% This reading (overdetermined_collapse) and its siblings (contingent_choice, hybrid_trigger) form a constraint family decomposing the transition_causality kernel. Each reading instantiates a distinct constraint with different ε, victim structure, and classification. The overdetermined reading claims mountain (Triffin as natural law); the contingent reading claims snare (policy capture); the hybrid claims tangled_rope (coordination + extraction + trigger).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
