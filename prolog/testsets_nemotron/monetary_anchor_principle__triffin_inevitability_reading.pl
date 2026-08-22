% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__triffin_inevitability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__triffin_inevitability_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: monetary_anchor_principle__triffin_inevitability_reading
 *   human_readable: Triffin Dilemma: Gold Reserve Exhaustion Under Fixed-Price Liquidity Provision
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story instantiates the Triffin Inevitability Reading of
 *   the monetary_anchor_principle kernel. The reading holds that the Bretton
 *   Woods gold-exchange standard (1944–1971) contained a structural
 *   contradiction: the US, as reserve currency issuer, had to run persistent
 *   current account deficits to supply global dollar liquidity, but those
 *   deficits steadily eroded US gold reserves at the fixed $35/oz price. The
 *   arithmetic is inescapable — gold stock is finite, liquidity demand grows
 *   with world trade, and the fixed parity means the US cannot simultaneously
 *   maintain convertibility and supply liquidity. No policy choice could
 *   resolve this; the system's design made its collapse a mathematical
 *   certainty. The victim is the Bretton Woods institutional framework
 *   itself, which dissolved when the constraint became binding. No agent
 *   benefits from the constraint's operation — it is a system-level failure,
 *   not a transfer mechanism.
 *
 * KEY AGENTS:
 *   - bretton_woods_institutional_framework: Victim (institutional/powerless) — the arrangement itself collapses when the constraint binds
 *   - us_treasury_fed: Agenda setter (institutional/analytical) — administers the system but cannot escape the constraint; the constraint acts ON them
 *   - global_dollar_holders: Payer (organized/mobile) — bear the risk of devaluation but cannot exit the dollar system
 *   - analytical_observer: Observer (analytical/analytical) — sees the structural necessity from outside the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__triffin_inevitability_reading, 0.05).
domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, 0.1).
domain_priors:theater_ratio(monetary_anchor_principle__triffin_inevitability_reading, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__triffin_inevitability_reading, mountain).
narrative_ontology:human_readable(monetary_anchor_principle__triffin_inevitability_reading, "Triffin Dilemma: Gold Reserve Exhaustion Under Fixed-Price Liquidity Provision").
narrative_ontology:topic_domain(monetary_anchor_principle__triffin_inevitability_reading, "monetary_economics/political_economy/international_finance").

domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__triffin_inevitability_reading, '497d081e-b957-4fd8-b2de-3f7963a04566').
narrative_ontology:cs_kernel_codification('497d081e-b957-4fd8-b2de-3f7963a04566', formalized).
narrative_ontology:cs_authority_grounding('497d081e-b957-4fd8-b2de-3f7963a04566', extraction).
narrative_ontology:cs_interpretation_layer_present('497d081e-b957-4fd8-b2de-3f7963a04566').
narrative_ontology:cs_reading_relation('497d081e-b957-4fd8-b2de-3f7963a04566', monetary_anchor_principle__punctuated_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('497d081e-b957-4fd8-b2de-3f7963a04566', monetary_anchor_principle__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('497d081e-b957-4fd8-b2de-3f7963a04566', foundational, triffin_contradiction_is_mathematical_necessity).
narrative_ontology:cs_axiom_status(triffin_contradiction_is_mathematical_necessity, holdable).
narrative_ontology:cs_axiom_grounding('497d081e-b957-4fd8-b2de-3f7963a04566', triffin_contradiction_is_mathematical_necessity, empirically_contingent).
narrative_ontology:cs_axiom('497d081e-b957-4fd8-b2de-3f7963a04566', foundational, no_policy_path_sustains_convertibility_past_binding_point).
narrative_ontology:cs_axiom_status(no_policy_path_sustains_convertibility_past_binding_point, holdable).
narrative_ontology:cs_axiom_grounding('497d081e-b957-4fd8-b2de-3f7963a04566', no_policy_path_sustains_convertibility_past_binding_point, empirically_contingent).
narrative_ontology:cs_reference_frame('497d081e-b957-4fd8-b2de-3f7963a04566', bretton_woods_gold_exchange_standard_as_designed_1944).
narrative_ontology:cs_drift_state('497d081e-b957-4fd8-b2de-3f7963a04566', gold_pool_collapse_1968, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('497d081e-b957-4fd8-b2de-3f7963a04566', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(monetary_anchor_principle__triffin_inevitability_reading, global_dollar_holders).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, triffin_dilemma_mathematical_necessity).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__triffin_inevitability_reading, fixed_price_gold_standard_liquidity_contradiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The institutional arrangement itself — fixed parities, IMF surveillance, gold convertibility — bears the full cost of the constraint. When the Triffin contradiction binds, the framework has no exit; it dissolves. It cannot reform from within because the contradiction is in its founding arithmetic. It is trapped by its own design.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, bretton_woods_institutional_framework, payer,
    institutional, generational, trapped, global).

% Administers the reserve currency role. Must run deficits to supply liquidity but watches gold reserves decline. Can intervene (gold pool, swap lines, capital controls) but cannot alter the arithmetic. Exit options are constrained: closing the gold window (1971) ends the system but is the only structural exit. They experience the constraint as an external limit, not a choice.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, us_treasury_fed, agenda_setter,
    institutional, biographical, constrained, global).

% Central banks and private holders of dollar reserves. Bear the risk that dollars will be devalued relative to gold. Can diversify (into gold, deutschmarks, SDRs) but the dollar system's scale makes full exit costly. Their mobility is real but incomplete — the dollar is the only reserve asset deep enough for global trade.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, global_dollar_holders, payer,
    organized, biographical, mobile, global).

% Economic historians, monetary theorists, and policymakers analyzing the system from outside. They see the structural necessity clearly but have no position within the constraint. Their role is to diagnose, not to act within the system.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__triffin_inevitability_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__triffin_inevitability_reading, diffuse).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__triffin_inevitability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods system coordinated global trade and reconstruction by providing a stable dollar-gold anchor and a multilateral payments framework. The Triffin dilemma is not a coordination function — it is the structural flaw that made the coordination unsustainable.
% TRANSFER_FUNCTION: No transfer function operates within this reading. The constraint does not move value from A to B; it destroys the arrangement that enabled transfers. The 'transfer' is the system's own dissolution — all parties lose the coordination benefits simultaneously.
% ABSENT_VOICES: Future generations who inherited the post-1971 fiat system are absent. They would ask whether the inevitability narrative was used to avoid harder questions: why was the adjustment burden placed entirely on the US? Why were symmetric adjustment mechanisms (Keynes's bancor) rejected in 1944? The excluded voices are those who would trace the 'natural law' back to a political choice at Bretton Woods.
% DISAPPEARANCE_RATIONALE: If the Triffin constraint disappeared (i.e., if gold were not finite, or liquidity demand did not grow, or the price were flexible), the Bretton Woods system could have continued indefinitely. The constraint's binding is what forced the world to rearrange into the fiat dollar standard. The world rearranged BECAUSE the constraint existed and became binding.
% FOUNDING_PROBLEM: The interwar monetary chaos: competitive devaluations, trade collapse, beggar-thy-neighbor policies, and the failure of the gold standard to provide stable exchange rates for reconstruction and trade.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (stable monetary framework for global trade) is attested as still live by every major central bank, the IMF, the WTO, and the G20 — all of which maintain institutions built to solve it. The Triffin reading does not claim the problem was solved; it claims the *arrangement* was mathematically doomed. Corroboration from outside the beneficiary set: the 'beneficiary set' is empty in this reading, so the corroboration is the entire postwar institutional architecture that still treats stable exchange rates as a live problem.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__triffin_inevitability_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__triffin_inevitability_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__triffin_inevitability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(monetary_anchor_principle__triffin_inevitability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__triffin_inevitability_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, ExtMetricName, E),
    domain_priors:suppression_score(monetary_anchor_principle__triffin_inevitability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(monetary_anchor_principle__triffin_inevitability_reading),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(monetary_anchor_principle__triffin_inevitability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(monetary_anchor_principle__triffin_inevitability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is near-zero (0.05) because the constraint does not transfer value from one agent to another — it is a structural impossibility that destroys the system containing it. Suppression is low (0.1) because no active enforcement is needed; the constraint enforces itself through arithmetic. Theater ratio is negligible (0.02) — there is no performative maintenance of a failed function. Accessibility collapse is near-total (0.95): once the Triffin logic is understood, no alternative policy within the gold standard framework can avoid the outcome. Resistance is minimal (0.05): the system's participants (US, Europe, IMF) all tried to manage the dilemma but could not overcome the mathematical contradiction. The claimed type is mountain because the constraint is a physical/logical limit — gold finitude + fixed price + growing liquidity demand = inevitable exhaustion.
 *
 * PERSPECTIVAL GAP:
 *   All seats experience this as a mountain — the US cannot choose to avoid the deficit, dollar holders cannot choose to avoid the risk, the IMF cannot choose to avoid the redesign. The engine will compute near-identical effective extraction (χ ≈ 0) for all seats because directionality is near-symmetric (d ≈ 0.5) and base extraction is negligible. The only seat divergence is temporal: early participants (1944–1958) experienced the constraint as latent coordination; late participants (1968–1971) experienced it as binding impossibility. The engine's time-indexed classification would capture this if the interval were split.
 *
 * DIRECTIONALITY LOGIC:
 *   No agent is a net beneficiary of the constraint's operation. The US gains seigniorage from dollar issuance but loses gold reserves — the net position is structurally constrained, not chosen. Europe gains export markets but accumulates depreciating dollar claims. The constraint is a negative-sum trap: all parties lose when it binds. Directionality for all structural agents is symmetric (d ≈ 0.5) — costs and benefits are balanced by the constraint's own logic. The Bretton Woods framework is the victim because its continued existence is mathematically impossible once the constraint activates.
 *
 * MANDATROPHY ANALYSIS:
 *   The Bretton Woods system was built to solve the interwar coordination failure (competitive devaluation, trade collapse). By 1968, the founding problem (stable exchange rates for trade) was still live, but the arrangement built to solve it had become mathematically impossible. The constraint did not persist after its function atrophied — it destroyed its host arrangement. This is not mandatrophy (a scaffold that outlives its purpose); it is a mountain that the scaffold was built on and could not survive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the monetary anchor principle a single contested kernel with multiple readings, or are these structurally distinct constraints?',
    'Test whether the three readings share a single ε referent: the 1944–1971 Bretton Woods gold-exchange standard arrangement. If changing the reading changes the ε referent (e.g., overdetermined reading includes policy choices the Triffin reading excludes), they are separate constraints, not readings of one kernel.',
    'If separate constraints, decompose into three independent stories linked via network.affects_constraints. If one kernel, the three readings are distinct indexical classifications of the same arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel_id groups one constraint with three readings or three distinct constraints').

omega_variable(
    natural_law_vs_policy_choice_ambiguity,
    'Is the gold reserve exhaustion a genuine physical/mathematical limit, or does the Triffin reading''s ''inevitability'' frame conceal policy choices (sterilization, capital controls, SDR creation) that could have extended the system?',
    'Counterfactual simulation: model Bretton Woods with alternative policy paths (e.g., tighter capital controls, earlier SDR issuance, symmetrical adjustment burden). If any path sustains convertibility past 1971 without violating gold parity, the ''inevitability'' claim is a false mountain — a constructed constraint presented as natural law.',
    'If policy paths exist, the constraint is a false summit mountain (FSM candidate): appears natural but benefits identifiable agents (US seigniorage, European export sectors) who preferred the narrative of inevitability to the cost of adjustment. Triggers FSM reclassification to tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_policy_choice_ambiguity, empirical, 'Whether the Triffin dilemma is a genuine Mountain or a False Summit Mountain with beneficiaries').

omega_variable(
    beneficiary_structure_of_inevitability_narrative,
    'Who benefits from framing the 1971 transition as structurally inevitable rather than a policy choice?',
    'Trace the political economy of the inevitability claim: US Treasury/Fed officials who avoided domestic adjustment, European central banks that accumulated dollar reserves without revaluation, academic economists whose models treated the dilemma as exogenous. If beneficiaries exist, the mountain claim carries extraction via narrative capture.',
    'Identifies whether the ''no beneficiary'' declaration is accurate or whether the inevitability narrative itself functions as extraction — shielding agents from accountability by naturalizing a contingent outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_of_inevitability_narrative, empirical, 'Whether the inevitability framing has identifiable beneficiaries').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__triffin_inevitability_reading, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triffin_inevitability_tr_t1944, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1944, 0.01).
narrative_ontology:measurement(triffin_inevitability_tr_t1950, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1950, 0.01).
narrative_ontology:measurement(triffin_inevitability_tr_t1958, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1958, 0.01).
narrative_ontology:measurement(triffin_inevitability_tr_t1960, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1960, 0.02).
narrative_ontology:measurement(triffin_inevitability_tr_t1965, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1965, 0.02).
narrative_ontology:measurement(triffin_inevitability_tr_t1968, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1968, 0.02).
narrative_ontology:measurement(triffin_inevitability_tr_t1971, monetary_anchor_principle__triffin_inevitability_reading, theater_ratio, 1971, 0.02).

% Extraction over time
narrative_ontology:measurement(triffin_inevitability_be_t1944, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1944, 0.02).
narrative_ontology:measurement(triffin_inevitability_be_t1950, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1950, 0.02).
narrative_ontology:measurement(triffin_inevitability_be_t1958, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1958, 0.03).
narrative_ontology:measurement(triffin_inevitability_be_t1960, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1960, 0.04).
narrative_ontology:measurement(triffin_inevitability_be_t1965, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1965, 0.04).
narrative_ontology:measurement(triffin_inevitability_be_t1968, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1968, 0.05).
narrative_ontology:measurement(triffin_inevitability_be_t1971, monetary_anchor_principle__triffin_inevitability_reading, base_extractiveness, 1971, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(triffin_inevitability_su_t1944, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1944, 0.05).
narrative_ontology:measurement(triffin_inevitability_su_t1950, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1950, 0.05).
narrative_ontology:measurement(triffin_inevitability_su_t1958, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1958, 0.07).
narrative_ontology:measurement(triffin_inevitability_su_t1960, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1960, 0.08).
narrative_ontology:measurement(triffin_inevitability_su_t1965, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1965, 0.09).
narrative_ontology:measurement(triffin_inevitability_su_t1968, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1968, 0.1).
narrative_ontology:measurement(triffin_inevitability_su_t1971, monetary_anchor_principle__triffin_inevitability_reading, suppression_requirement, 1971, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__triffin_inevitability_reading, global_infrastructure).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__punctuated_swap_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__triffin_inevitability_reading, monetary_anchor_principle__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family: monetary_anchor_principle kernel decomposed into three readings. This reading (triffin_inevitability) has ε ≈ 0.05 (structural necessity, no extraction). punctuated_swap_reading has higher ε (institutional choice extracts from dollar holders). overdetermined_composite_reading has intermediate ε (policy choices layered on structural pressure). The three readings share the referent (1944–1971 arrangement) but author different ε because they identify different causal structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
