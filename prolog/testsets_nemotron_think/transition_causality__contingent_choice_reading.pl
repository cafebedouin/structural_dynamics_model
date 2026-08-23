% ============================================================================
% CONSTRAINT STORY: transition_causality__contingent_choice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Contingent Choice Reading of the 1971 Monetary Transition
 *   domain: monetary_economics/political_economy/international_finance
 *
 * SUMMARY:
 *   This constraint story models the 'contingent choice' reading of the 1971
 *   monetary transition as a discursive constraint in monetary economics and
 *   political economy. The reading asserts that Nixon's suspension of gold
 *   convertibility was a policy decision that could have been avoided — a
 *   contingent act of agency rather than structural collapse. It operates as
 *   a coordination device for U.S. monetary sovereignty narratives while
 *   extracting explanatory authority from structuralist accounts. The
 *   constraint has persisted for five decades, with extraction and
 *   suppression gradually increasing as the reading became institutionalized
 *   in central bank curricula and policy discourse.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(transition_causality__contingent_choice_reading, 0.38).
domain_priors:suppression_score(transition_causality__contingent_choice_reading, 0.45).
domain_priors:theater_ratio(transition_causality__contingent_choice_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(transition_causality__contingent_choice_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(transition_causality__contingent_choice_reading, tangled_rope).
narrative_ontology:human_readable(transition_causality__contingent_choice_reading, "Contingent Choice Reading of the 1971 Monetary Transition").
narrative_ontology:topic_domain(transition_causality__contingent_choice_reading, "monetary_economics/political_economy/international_finance").

domain_priors:requires_active_enforcement(transition_causality__contingent_choice_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(transition_causality__contingent_choice_reading, 'b1d44556-6c16-4598-9dab-9159439863a7').
narrative_ontology:cs_kernel_codification('b1d44556-6c16-4598-9dab-9159439863a7', distributed).
narrative_ontology:cs_authority_grounding('b1d44556-6c16-4598-9dab-9159439863a7', distributed).
narrative_ontology:cs_reading_relation('b1d44556-6c16-4598-9dab-9159439863a7', transition_causality__overdetermined_collapse_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1d44556-6c16-4598-9dab-9159439863a7', transition_causality__hybrid_trigger_reading, coexists_with).
narrative_ontology:cs_axiom('b1d44556-6c16-4598-9dab-9159439863a7', foundational, nixon_decision_as_primary_causal_node).
narrative_ontology:cs_axiom_status(nixon_decision_as_primary_causal_node, holdable).
narrative_ontology:cs_axiom_grounding('b1d44556-6c16-4598-9dab-9159439863a7', nixon_decision_as_primary_causal_node, empirically_contingent).
narrative_ontology:cs_axiom('b1d44556-6c16-4598-9dab-9159439863a7', secondary, counterfactual_viability_high).
narrative_ontology:cs_axiom_status(counterfactual_viability_high, holdable).
narrative_ontology:cs_axiom_grounding('b1d44556-6c16-4598-9dab-9159439863a7', counterfactual_viability_high, empirically_contingent).
narrative_ontology:cs_reference_frame('b1d44556-6c16-4598-9dab-9159439863a7', contingent_policy_choice_frame).
narrative_ontology:cs_drift_state('b1d44556-6c16-4598-9dab-9159439863a7', contemporary_scholarship, gap(stable, minor, false)).
narrative_ontology:cs_created_at('b1d44556-6c16-4598-9dab-9159439863a7', '').
narrative_ontology:cs_kernel_id(transition_causality__contingent_choice_reading, transition_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, us_policymakers).
narrative_ontology:constraint_beneficiary(transition_causality__contingent_choice_reading, agency_emphasizing_scholars).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, structuralist_scholars).
narrative_ontology:constraint_victim(transition_causality__contingent_choice_reading, countries_seeking_exchange_stability).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, policy_agency_over_structural_determinism).
narrative_ontology:constraint_vindicates(transition_causality__contingent_choice_reading, us_monetary_sovereignty_justification).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain retrospective justification for the 1971 Nixon shock as a deliberate exercise of sovereignty rather than a forced collapse. This reading legitimizes current U.S. monetary autonomy and insulates policymakers from claims that the transition was inevitable structural failure.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, us_policymakers, beneficiary,
    institutional, biographical, arbitrage, global).

% Academic and policy economists whose work centers human agency and decision-contingency in monetary history. The reading validates their methodological framework and secures citation networks, grant funding, and policy advisory roles.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, agency_emphasizing_scholars, beneficiary,
    organized, generational, mobile, global).

% Scholars emphasizing Triffin dilemma, global dollar shortage, and irreconcilable fixed-rate contradictions. Their explanatory framework is marginalized in mainstream discourse, reducing publication venues, policy influence, and graduate training pipelines.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, structuralist_scholars, payer,
    organized, generational, constrained, global).

% Emerging market and smaller advanced economies that bear the volatility costs of the post-Bretton Woods floating regime. The contingent-choice reading frames the resulting instability as a policy choice that could have been avoided, shifting blame from structural inevitability to U.S. decisions — but without delivering a viable alternative regime.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, countries_seeking_exchange_stability, payer,
    moderate, biographical, constrained, regional).

% Central bank research departments, IMF/World Bank research units, top economics journal editors, and major financial press. They set the acceptable boundaries of the transition narrative, enforce citation norms, and control access to policy-relevant platforms.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, policy_discourse_gatekeepers, agenda_setter,
    institutional, generational, analytical, global).

% Historians of monetary architecture who document the contingency without taking a normative stance. They provide archival evidence (Nixon tapes, Treasury memos) that both supports and complicates the reading, but lack institutional power to shape the dominant policy narrative.
narrative_ontology:constraint_stakeholder(transition_causality__contingent_choice_reading, independent_historians, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a policy-usable narrative that the end of Bretton Woods was a deliberate U.S. choice, enabling a coherent account of monetary sovereignty that legitimizes the floating-rate regime and U.S. discretionary policy space.
% TRANSFER_FUNCTION: Moves explanatory authority and policy legitimacy from structuralist accounts (Triffin dilemma, global imbalances) to agency-centered accounts, concentrating interpretive capital among U.S. policymakers and their aligned scholars while marginalizing structuralist alternatives.
% ABSENT_VOICES: Non-U.S. monetary authorities (especially European and Japanese officials of the 1970s) who experienced the transition as structural necessity, not choice; Global South economists who view the floating regime as imposed instability; earlier structuralist literature (Triffin, Kindleberger) systematically under-cited in post-1971 mainstream curricula.
% DISAPPEARANCE_RATIONALE: If the contingent-choice reading vanished, the dominant narrative would revert to structural inevitability, undermining the legitimacy of U.S. monetary discretion, altering policy lessons taught to central bankers, and reopening the case for a rules-based international monetary reform.
% FOUNDING_PROBLEM: The need to explain the 1971 Nixon shock in a way that preserves U.S. agency and justifies the resulting floating rate system as a deliberate policy achievement rather than a systemic failure.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by Nixon's own tapes and contemporaneous Treasury memos showing active deliberation among Volcker, Shultz, and Connally; contested by economists who point to irreconcilable structural pressures (Triffin dilemma, cumulative reserve losses, speculator attacks) that made some break inevitable regardless of decision-makers' intentions.
narrative_ontology:disappearance_verdict(transition_causality__contingent_choice_reading, world_rearranges).
narrative_ontology:founding_problem_status(transition_causality__contingent_choice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(transition_causality__contingent_choice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(transition_causality__contingent_choice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(transition_causality__contingent_choice_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(transition_causality__contingent_choice_reading_tests).
:- end_tests(transition_causality__contingent_choice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the reading's capture of interpretive capital: it redirects explanatory authority toward agency-centered frameworks that benefit U.S. policymakers and aligned scholars. Suppression (0.45) measures the marginalization of structuralist alternatives in top journals, policy advisory roles, and graduate training — not total exclusion but systematic disadvantage. Theater ratio (0.22) is low because the reading does perform genuine coordination: it provides a usable policy narrative that enables decisive action in crises. Accessibility collapse (0.35) is moderate: structuralist alternatives persist in heterodox circles and historical literature but are excluded from the mainstream policy canon. Resistance (0.48) reflects ongoing contestation from structuralist scholars and non-U.S. policy circles.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (gatekeepers), the constraint appears as a rope: genuine coordination of a usable policy narrative with minimal coercion. From the payer seats (structuralist scholars, stability-seeking countries), it computes as a snare: extraction of explanatory authority and policy legitimacy through systematic suppression of alternatives. The engine will compute this divergence from the structural data — the claimed_type (tangled_rope) captures the hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   U.S. policymakers and agency-emphasizing scholars are structural beneficiaries (d near 0.0): the reading subsidizes their authority and policy space. Structuralist scholars and stability-seeking countries are targets (d near 1.0): they bear the cost of marginalization and regime instability. Policy discourse gatekeepers sit near symmetric (d ~0.5): they both enforce and are constrained by the reading. Independent historians are analytical observers (d = 0.5 by definition). Exit options differentiate: U.S. policymakers have arbitrage-grade exit (they could adopt another narrative without career cost); structuralist scholars are constrained (institutional incentives penalize their framework); stability-seeking countries are trapped (no exit from dollar system volatility).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (justifying U.S. monetary sovereignty post-1971) remains live — the floating regime still requires legitimation. However, the reading's coordination function has atrophied: the original contingency claim (that different choices were viable) is now used to block consideration of new structural reforms (e.g., managed exchange rate systems, SDR-based reserves). The constraint persists not because it solves a current coordination problem but because it protects the incumbent policy framework. This is mandatrophy: the mandate (justify the transition) has outlived its function, but the constraint remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contingency_vs_retrospective_justification,
    'Is the contingency claim a genuine historical reading supported by archival evidence, or a retrospective justification for U.S. hegemony constructed after the fact?',
    'Comparative analysis of contemporaneous policy memos vs. later memoirs and institutional histories; counterfactual simulation of Bretton Woods with alternative policy choices (capital controls, parity changes, SDR activation).',
    'If retrospective justification, the reading''s extraction is ideological legitimation rather than historical coordination — reclassify toward snare. If genuine contingency, the coordination function is real and the extraction is the price of a usable policy narrative — tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingency_vs_retrospective_justification, empirical, 'Whether the contingency claim reflects historical reality or retrospective myth-making.').

omega_variable(
    structural_inevitability_threshold,
    'At what threshold of structural pressure does a policy choice cease to be ''contingent'' and become ''forced''?',
    'Formal modeling of Bretton Woods constraints (Triffin dilemma, gold coverage ratio, speculative attack dynamics) to identify the point of no return; historical comparison to other fixed-rate regimes that collapsed without a single decision node.',
    'If structural pressures made collapse inevitable by 1968-69, the contingency claim is false and the reading is a snare. If viable alternatives existed through 1971, the reading''s coordination function is genuine.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_inevitability_threshold, conceptual, 'The conceptual boundary between contingent choice and structural necessity in monetary regime collapse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(transition_causality__contingent_choice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(transition_causality__contingent_choice_reading_tr_t0, transition_causality__contingent_choice_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_tr_t10, transition_causality__contingent_choice_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_tr_t20, transition_causality__contingent_choice_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_tr_t30, transition_causality__contingent_choice_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_tr_t40, transition_causality__contingent_choice_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_tr_t50, transition_causality__contingent_choice_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(transition_causality__contingent_choice_reading_be_t0, transition_causality__contingent_choice_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_be_t10, transition_causality__contingent_choice_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_be_t20, transition_causality__contingent_choice_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_be_t30, transition_causality__contingent_choice_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_be_t40, transition_causality__contingent_choice_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_be_t50, transition_causality__contingent_choice_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(transition_causality__contingent_choice_reading_su_t0, transition_causality__contingent_choice_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_su_t10, transition_causality__contingent_choice_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_su_t20, transition_causality__contingent_choice_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_su_t30, transition_causality__contingent_choice_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_su_t40, transition_causality__contingent_choice_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(transition_causality__contingent_choice_reading_su_t50, transition_causality__contingent_choice_reading, suppression_requirement, 50, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(transition_causality__contingent_choice_reading, information_standard).
narrative_ontology:boltzmann_floor_override(transition_causality__contingent_choice_reading, 0.03).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, bretton_woods_collapse).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, floating_rate_regime_legitimacy).
narrative_ontology:affects_constraint(transition_causality__contingent_choice_reading, us_monetary_hegemony_narrative).

% DUAL FORMULATION NOTE:
% This reading is one of three in the transition_causality kernel family. It emphasizes agency over structure. The overdetermined_collapse_reading emphasizes structural inevitability. The hybrid_trigger_reading synthesizes both. All three compete for explanatory authority in monetary history and policy discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, institutional, 0.1).
constraint_indexing:directionality_override(transition_causality__contingent_choice_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
