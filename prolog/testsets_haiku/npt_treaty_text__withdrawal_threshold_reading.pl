% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_withdrawal_threshold, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold: Regime Stability vs. Sovereignty Preservation
 *   domain: international/legal/security
 *
 * SUMMARY:
 *   The NPT's Article X permits withdrawal on 90 days' notice if
 *   'extraordinary events' jeopardize supreme interests. The text is
 *   ambiguous: it does not define 'extraordinary' or specify who adjudicates.
 *   This reading instantiates the high-threshold interpretation—the NWS-led
 *   understanding that withdrawal is only legitimate under extreme, nearly
 *   existential circumstances, with full diplomatic justification and
 *   exposure to international pressure. This reading benefits threshold
 *   states by preserving their exit option's credibility, but extracts from
 *   those actually considering or executing withdrawal (North Korea, Iran,
 *   potentially Japan/South Korea in crisis). The founding problem
 *   (preventing proliferation while preserving sovereignty) is contested: NWS
 *   say the problem is live and the regime solves it; low-threshold advocates
 *   say the problem is overstated and the regime's extractions are
 *   unjustified.
 *
 * KEY AGENTS:
 *   - nws_establishment: NWS collective, administers and enforces the high-threshold reading; institutional power, no exit option (the constraint is their creation)
 *   - threshold_states: Japan, South Korea, Iran, Brazil; powerful, benefit from the exit option's existence even if never used; constrained exit (regional security dependencies)
 *   - withdrawal_exercisers: North Korea (actual 2003), Iran (post-JCPOA crisis), potential others; organized, highly constrained; bear extraction cost of legitimacy challenges and sanctions
 *   - low_threshold_advocates: non-aligned movement, disarmament NGOs, some NNWS; excluded from authority, politically active but institutionally subordinated
 *   - treaty_depositary_authority: UN Secretary-General; administers notifications but defers to NWS interpretation in practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.68).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold: Regime Stability vs. Sovereignty Preservation").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international/legal/security").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '8256e7c5-ba53-45b8-8cff-4694da51e89f').
narrative_ontology:cs_kernel_codification('8256e7c5-ba53-45b8-8cff-4694da51e89f', fixed_text).
narrative_ontology:cs_authority_grounding('8256e7c5-ba53-45b8-8cff-4694da51e89f', extraction).
narrative_ontology:cs_interpretation_layer_present('8256e7c5-ba53-45b8-8cff-4694da51e89f').
narrative_ontology:cs_reading_relation('8256e7c5-ba53-45b8-8cff-4694da51e89f', npt_treaty_text__nws_reading, influences).
narrative_ontology:cs_reading_relation('8256e7c5-ba53-45b8-8cff-4694da51e89f', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_axiom('8256e7c5-ba53-45b8-8cff-4694da51e89f', foundational, article_x_extraordinariness_requires_existential_justification).
narrative_ontology:cs_axiom_status(article_x_extraordinariness_requires_existential_justification, holdable).
narrative_ontology:cs_axiom_grounding('8256e7c5-ba53-45b8-8cff-4694da51e89f', article_x_extraordinariness_requires_existential_justification, conventional).
narrative_ontology:cs_axiom('8256e7c5-ba53-45b8-8cff-4694da51e89f', foundational, regime_stability_priority_over_individual_sovereignty).
narrative_ontology:cs_axiom_status(regime_stability_priority_over_individual_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('8256e7c5-ba53-45b8-8cff-4694da51e89f', regime_stability_priority_over_individual_sovereignty, instrumental).
narrative_ontology:cs_reference_frame('8256e7c5-ba53-45b8-8cff-4694da51e89f', article_x_ambiguous_extraordinary_events).
narrative_ontology:cs_drift_state('8256e7c5-ba53-45b8-8cff-4694da51e89f', north_korea_2003_through_2024, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8256e7c5-ba53-45b8-8cff-4694da51e89f', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, nws_regime_stability).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, withdrawal_exercisers).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, nnws_crisis_actors).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (1968, treaty text ambiguous, NWS interpretation nascent) to 0.68 (2024, high-threshold interpretation enforced through North Korea precedent and Iran case studies). North Korea's 2003 withdrawal announcement (later reversed) crystallized the NWS consensus: withdrawal is legitimate only if justified by overwhelming security crisis and sustained against international pressure. The measurement trajectory tracks the accumulation of enforcement machinery—not written rules but diplomatic practice, sanctions, and legitimacy denial. Theater ratio rises from 0.18 to 0.42: early years the regime emphasized disarmament coordination language; by 2024, a growing share of NPT review conference energy goes to defending the withdrawal interpretation and managing its contradictions with sovereignty rhetoric. Suppression requirement (the active force needed to keep states bound) rises from 0.42 to 0.71: initially the regime's normative appeal and Cold War strategic logic were enough; by 2024, threat of sanctions, alliance pressure, and diplomatic isolation are necessary to hold threshold states in the regime despite their doubts. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, this is successful coordination: the regime has prevented dozens of potential nuclear states from acquiring weapons, reduced the number of nuclear states from a projected 20+ to 9, and created inspection infrastructure (IAEA). The high threshold is the enforcement cost of this success. From the threshold-state seat, this is asymmetric negotiation: we accept constraints on our nuclear development in exchange for NWS disarmament commitments (Article VI) that have never materialized, and the exit option's credibility is our only leverage—the high threshold is the extraction price for that leverage. From the low-threshold advocate seat, this is false coordination masking NWS hegemony: the regime extracts developing states' non-proliferation in exchange for nominal exit rights that are practically unexercisable. The engine should compute these divergences from the structural data. The constraint's claimed type is tangled_rope, but from some seats it may compute as snare (extractive, heavily enforcement-dependent, alternative suppressed). This divergence is what the corpus exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS and regime beneficiaries (nws_establishment, non_state_security_actors) sit at low directionality (~0.1–0.2): they collect from the regime's operation and face no exit cost. Threshold states sit near symmetric (~0.45–0.55): they benefit from preserving their exit option's credibility but pay the cost of being continuously monitored and constrained; their exit option's value depends on others believing they could use it, which requires the high threshold to credibly exist. Withdrawal exercisers sit at high directionality (~0.8–0.9): they pay extraction (legitimacy costs, sanctions, isolation) if they exercise the nominal right. Crisis actors face identity-locked exit: their security identity is entangled with the regime (they are defined as 'NPT-bound non-nuclear states' in alliance security calculations), so actual exit, though permitted, is identity-shattering. Low-threshold advocates have analytical directionality (~0.5, observing the constraint's operation but neither collecting nor paying directly). The depositary sits near agenda-setter but with reduced actual power (d~0.35): they administer procedure but do not independently judge legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—preventing proliferation while respecting sovereignty—has two contradictory resolutions in this reading. The high-threshold interpretation prioritizes regime stability (preventing proliferation) at the cost of making nominal sovereignty commitments (Article X) effectively non-exercisable. This classification as tangled_rope (not snare, not pure rope) depends on whether the coordination benefit (non-proliferation infrastructure) is genuine. If non-proliferation is a real collective action problem (many states face incentives to defect if others do), then the high threshold is the enforcement cost of that coordination, and the constraint is legitimately tangled. If non-proliferation is instead a cover story for NWS domination (the real point is keeping nuclear weapons concentrated in five hands), then the coordination story evaporates and the constraint reclassifies as snare. The measurement trajectory and seat-divergence analysis do not resolve this—they document the empirical operation. The resolution hinges on whether the low-threshold advocates' critique is right: is the NPT coordination actually solving a live proliferation problem, or is it an extraction mechanism dressed as coordination? This ambiguity is the constitutive feature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_hegemony_reading,
    'Is the NPT non-proliferation regime a genuine solution to a collective action problem (states have incentives to defect if others do, so coordination is necessary), or is it hegemonic NWS control dressed as coordination?',
    'Historical counterfactual analysis: without the NPT and high-threshold Article X, would proliferation have accelerated dramatically, or would regional security dynamics produce roughly the same five-state nuclear landscape? Model-based analysis of incentive structures for states to pursue nuclear weapons absent the regime.',
    'If coordination is genuine, the tangled_rope classification holds: extraction (the high threshold, the cost to exit) is the necessary enforcement machinery for genuine collective benefit (non-proliferation). If hegemonic, the classification reclassifies to snare: the coordination story is cover; persistence depends entirely on coercion and enforcement; alternatives (easy exit, regional nuclear deterrence, open proliferation) are suppressed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_hegemony_reading, conceptual, 'Whether the NPT solves a real collective action problem or is NWS-led extraction.').

omega_variable(
    extraordinary_events_definition_ambiguity,
    'Does the treaty text''s ''extraordinary events'' clause include regional security crises (conventional invasion, blockade, existential ally abandonment), or only global catastrophic scenarios (superpower war, nuclear exchange, planetary threat)?',
    'Formal treaty interpretation per Vienna Convention on the Law of Treaties (object and purpose, negotiating history, subsequent practice). Comparative case analysis: which state withdrawals or withdrawal threats have the international community recognized as meeting ''extraordinary events''? (North Korea''s 2003 claim was rejected; Iran''s post-JCPOA stance was disputed.)',
    'A narrow definition (only global catastrophe) means threshold states in regional crises have no legitimate exit, making the constraint more extractive (d increases for crisis actors). A broad definition (regional security crisis counts) reduces extraction but weakens regime stability. The measurement metrics would shift: lower suppression requirement under broad definition (fewer states need to be coerced), higher extraction (more states claim extraordinary events simultaneously).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraordinary_events_definition_ambiguity, empirical, 'Ambiguity in what conditions justify Article X withdrawal.').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For threshold states (especially Japan, South Korea) that self-identify as ''responsible nuclear-threshold states'' respecting international law, is the high-threshold interpretation structurally internalized (they have fused their identity with the regime) or structurally imposed (external sanctions and alliance pressure hold them in)?',
    'Post-exit scenario analysis: if a threshold state (say, Japan post-US withdrawal from East Asia) chose to withdraw and pursue nuclear weapons openly, how much of the suppressive force would persist from internal commitment vs. external pressure? Survey data from security elites in threshold states on whether they view Article X as a negotiable constraint or an internalized norm.',
    'If identity-locked (internalized), the suppression metric understates the constraint''s true hold—the state carries the suppression with them even if the regime changes. The constraint''s persistence depends partly on identity fusion, making exit more costly than formal measures show. If structurally imposed, suppression is accurate as measured; exit is easier if the external regime weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Whether threshold-state commitment to the NPT is internalized identity or external coercion.').

omega_variable(
    north_korea_precedent_lock_in,
    'Did North Korea''s 2003 withdrawal announcement lock in the high-threshold interpretation permanently, or was the high threshold always the de facto practice and North Korea''s announcement merely surfaced it?',
    'Archival analysis of pre-2003 NPT review conferences and treaty depositary records: what was the stated interpretation of Article X before North Korea announced? Did the interpretation change after 2003, or did enforcement intensity increase while the interpretation remained stable?',
    'If the 2003 precedent created the high-threshold interpretation, then the constraint is somewhat contingent—a different NWS response (acknowledging North Korea''s right to withdraw) would have produced a low-threshold regime. If the high threshold was always the practice, 2003 merely hardened what was already enforceable, and the constraint has deeper institutional roots. This affects whether the high threshold is an artifact of recent great-power fragmentation or a structural feature of NWS-led regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_lock_in, empirical, 'Whether North Korea''s 2003 withdrawal announcement created or merely surfaced the high-threshold interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1968, 0.18).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.65).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1968, 0.42).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.68).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__withdrawal_threshold_reading, 0.18).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% The NPT's Article X withdrawal threshold is one contestable element of the broader NPT kernel. This reading (withdrawal_threshold_reading: high threshold, regime stability priority) coexists with nws_reading (non-proliferation as binding, disarmament as aspirational) and nnws_reading (disarmament as binding, non-proliferation as conditional). All three readings instantiate different structural constraints from the same treaty text. This reading influences both siblings: a high-threshold Article X makes non-proliferation easier to enforce (supports nws_reading) and disarmament obligations easier to defer (undermines nnws_reading). The three stories should be generated separately per ε-invariance; this constraint story models Article X withdrawal dynamics specifically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
