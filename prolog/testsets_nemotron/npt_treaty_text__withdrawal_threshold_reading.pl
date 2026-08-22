% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Reading
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story captures one reading of the contested NPT Article X
 *   kernel: the high-threshold interpretation that prioritizes regime
 *   stability over sovereign exit. The reading treats "extraordinary events
 *   that have jeopardized the supreme interests of its country" as a
 *   demanding standard requiring objective demonstration, not subjective
 *   claim. The North Korea precedent (2003 withdrawal) is read as
 *   non-precedential because it was not challenged through the review
 *   conference mechanism. This reading benefits threshold states by
 *   preserving their strategic ambiguity while constraining exit-seekers. It
 *   is a tangled rope because it coordinates non-proliferation expectations
 *   (genuine function) while asymmetrically extracting exit flexibility from
 *   sovereignty-prioritizing states (extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.55).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.45).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '46c38923-6230-447c-a89a-53c058034945').
narrative_ontology:cs_kernel_codification('46c38923-6230-447c-a89a-53c058034945', fixed_text).
narrative_ontology:cs_authority_grounding('46c38923-6230-447c-a89a-53c058034945', lineage).
narrative_ontology:cs_interpretation_layer_present('46c38923-6230-447c-a89a-53c058034945').
narrative_ontology:cs_reading_relation('46c38923-6230-447c-a89a-53c058034945', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('46c38923-6230-447c-a89a-53c058034945', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_axiom('46c38923-6230-447c-a89a-53c058034945', foundational, withdrawal_requires_objective_extraordinary_events).
narrative_ontology:cs_axiom_status(withdrawal_requires_objective_extraordinary_events, holdable).
narrative_ontology:cs_axiom_grounding('46c38923-6230-447c-a89a-53c058034945', withdrawal_requires_objective_extraordinary_events, conventional).
narrative_ontology:cs_axiom('46c38923-6230-447c-a89a-53c058034945', secondary, regime_continuity_supersedes_sovereign_exit).
narrative_ontology:cs_axiom_status(regime_continuity_supersedes_sovereign_exit, holdable).
narrative_ontology:cs_axiom_grounding('46c38923-6230-447c-a89a-53c058034945', regime_continuity_supersedes_sovereign_exit, instrumental).
narrative_ontology:cs_reference_frame('46c38923-6230-447c-a89a-53c058034945', id_1968_drafting_compromise_ambiguity).
narrative_ontology:cs_drift_state('46c38923-6230-447c-a89a-53c058034945', post_2003_north_korea_withdrawal, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46c38923-6230-447c-a89a-53c058034945', '2026-08-03T14:22:10Z').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, regime_stability_advocates).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, sovereignty_prioritizing_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, exit_seeking_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States like Iran, Japan, and South Korea that possess latent nuclear capability but remain in the NPT. They benefit from an ambiguous withdrawal threshold that preserves their exit option credibility without triggering immediate proliferation cascades. The high-threshold interpretation maintains their strategic flexibility while keeping them within the regime's non-proliferation commitments.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    moderate, biographical, constrained, global).

% Institutional actors (IAEA, NPT review conference chairs, major NWS/NNWS coalition partners) who interpret Article X to require a high threshold for withdrawal — "extraordinary events" that jeopardize supreme interests. They administer the regime's continuity, set the interpretive agenda at review conferences, and benefit from a stable non-proliferation architecture. Their enforcement is diplomatic and institutional rather than coercive.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, regime_stability_advocates, agenda_setter,
    institutional, generational, analytical, global).

% States (historically North Korea, potentially others) that read Article X as preserving an unqualified sovereign right to withdraw on short notice. They bear the diplomatic costs of a high-threshold interpretation that constrains their exit freedom and exposes them to allegations of material breach. Their exit option is structurally mobile — they can withdraw, but the high-threshold reading raises the political and legal costs of doing so.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, sovereignty_prioritizing_states, payer,
    powerful, biographical, mobile, global).

% States in acute security crises that would withdraw immediately under a low-threshold reading but find the high-threshold interpretation a binding constraint. They bear the full cost of continued compliance when their security calculus demands exit. Their exit is trapped by the regime's collective interpretation and the threat of collective response.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, exit_seeking_states, payer,
    moderate, immediate, trapped, regional).

% The five NPT-recognized NWS benefit from a high withdrawal threshold that prevents cascade proliferation and preserves the regime that legitimizes their status. They simultaneously set the interpretive agenda through their privileged position in the review process and Security Council. Their exit option is arbitrage-grade — they are not bound by the same withdrawal constraints as NNWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, nuclear_weapon_states, agenda_setter).

% NNWS coalition that would object to a high-threshold reading as privileging regime stability over sovereign equality and Article VI disarmament obligations. They are structurally excluded from the interpretive consensus that emerges among the P5 and their allies. Their exit is constrained by development assistance and security guarantees tied to NPT membership.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_aligned_movement_states, excluded,
    organized, generational, constrained, global).

% Analytical observers who track the interpretive contest over Article X. They see the full structural ambiguity: the treaty text supports both readings, the North Korea precedent is contested, and the threshold ambiguity serves as a strategic stabilizer. They neither collect nor pay but map the constraint's operation across all seats.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a cascade of withdrawals that would collapse the non-proliferation regime by establishing a shared interpretive threshold for Article X invocation. The high-threshold reading coordinates expectations: states know that withdrawal requires demonstrating "extraordinary events," which creates a cooling-off period and channels disputes into the review conference process.
% TRANSFER_FUNCTION: Moves interpretive authority and exit flexibility from sovereignty-prioritizing states to regime-stability advocates. The high-threshold reading transfers the burden of proof to the withdrawing state (must prove supreme interests jeopardized) and transfers enforcement standing to the collective (can challenge withdrawal as non-compliant).
% ABSENT_VOICES: States in acute existential security crises that would withdraw immediately under a low-threshold reading but cannot articulate their position in the review conference process because doing so signals intent and triggers preventive pressure. Also absent: future proliferant states whose calculus is shaped by the threshold ambiguity but who have no seat at the interpretive table.
% DISAPPEARANCE_RATIONALE: If the high-threshold interpretation vanished overnight, the North Korea precedent would become the sole interpretive anchor — withdrawal on 90 days' notice for any "supreme interests" claim. Multiple threshold states (Iran, potentially others) would face immediate pressure to either withdraw or accept intrusive verification. The non-proliferation regime would lose its primary procedural brake on cascade proliferation.
% FOUNDING_PROBLEM: The NPT's original bargain required a withdrawal clause (Article X) to secure sovereign adherence, but the drafters left the threshold ambiguous to avoid either legitimizing easy exit or making the treaty a suicide pact. The founding problem: how to embed an exit option that preserves sovereignty without making the regime revocable at will.
% FOUNDING_PROBLEM_CORROBORATION: Treaty historians (e.g., Jayantha Dhanapala, George Bunn) attest the ambiguity was deliberate — a compromise between US/USSR preference for high threshold and non-aligned demand for sovereign exit. The NWS and regime-stability advocates claim the problem is live (proliferation risks persist). Sovereignty-prioritizing states and NAM attest the problem is dead for them — the disarmament bargain (Article VI) has failed, so the exit option's credibility is the only remaining sovereign guarantee.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.55) is moderate: the constraint extracts exit flexibility from states that would otherwise withdraw, but the extraction is not monetary or material — it is strategic option value. Suppression (0.45) is moderate: the constraint suppresses easy withdrawal through collective diplomatic pressure and legal challenge, not physical force. Theater (0.35) reflects that the review conference process performs constraint while the real enforcement is the threat of collective response. Accessibility collapse (0.4) is moderate: alternatives (withdrawal) exist but are politically costly. Resistance (0.5) reflects sustained contestation from sovereignty-prioritizing states and NAM.
 *
 * PERSPECTIVAL GAP:
 *   From the regime-stability seat, this is a rope: genuine coordination preventing cascade proliferation. From the sovereignty-prioritizing seat, it is a snare: extraction of sovereign exit rights under cover of regime maintenance. From the threshold-state seat, it is a tangled rope: they are coordinated (non-proliferation stability) but also pay (constrained strategic signaling). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Regime-stability advocates (institutional agenda-setters) sit at d ~ 0.1 — they benefit from the constraint's coordination function and administer it. Threshold states (moderate beneficiaries) sit at d ~ 0.25 — they gain strategic flexibility from the ambiguity. NWS (institutional beneficiaries/agenda-setters) sit at d ~ 0.1 — arbitrage-grade exit. Sovereignty-prioritizing states (powerful payers) sit at d ~ 0.7 — mobile but costly exit. Exit-seeking states (moderate payers) sit at d ~ 0.9 — trapped. NAM (organized excluded) sit at d ~ 0.6 — constrained exit, no voice. Scholars (analytical observers) sit at d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sovereign exit without regime suicide) remains contested. The high-threshold reading has not resolved the mandatrophy — it has institutionalized the ambiguity. If Article VI disarmament continues to stall, the reading may degrade into a piton (theatrical review conferences maintaining a constraint that no longer coordinates). If a cascade withdrawal occurs, it may snap into a snare (enforced non-withdrawal). The current tangled_rope classification captures the active coordination/extraction hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    north_korea_precedent_status,
    'Does the North Korea withdrawal (2003) establish a binding low-threshold precedent, or is it a non-precedential anomaly because it was not adjudicated through the review conference mechanism?',
    'A future withdrawal challenged at a review conference with a formal compliance finding would clarify the precedent''s weight. Absent that, the ambiguity persists.',
    'If binding precedent, the high-threshold reading collapses — extraction drops, the constraint becomes a rope or mountain. If non-precedential, the high-threshold reading maintains its tangled_rope character.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(north_korea_precedent_status, conceptual, 'Precedential status of the sole Article X invocation').

omega_variable(
    threshold_state_calculation,
    'Do threshold states (Iran, Japan, South Korea) actually benefit from the high-threshold ambiguity, or does it trap them in a compliance bind that increases their security costs?',
    'Comparative analysis of threshold-state behavior: do they invest in breakout capability because the threshold is high (trapped) or because it is low (credible exit)?',
    'If threshold states are net payers, the beneficiary declaration shifts — the constraint becomes more snare-like. If net beneficiaries, the tangled_rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_calculation, empirical, 'Whether threshold states are genuine beneficiaries or constrained payers').

omega_variable(
    committer_frame_disagreement_location,
    'Where exactly do the three NPT readings disagree structurally — on the beneficiary/victim assignment, the coordination function, the enforcement mechanism, or the founding problem status?',
    'Structural mapping of each reading''s stakeholder surface and metric profile. The disagreement is located in the withdrawal threshold interpretation, but its downstream effects on the entire regime''s classification differ per reading.',
    'If the disagreement is only on withdrawal threshold, the readings are loosely coupled. If it cascades into different beneficiary/victim structures across the regime, they are tightly coupled constraint family members.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_disagreement_location, conceptual, 'Structural location of the kernel''s reading-level disagreement').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (collective diplomatic pressure, Security Council resolutions) or internalized (states self-censor withdrawal intent due to normative internalization)?',
    'Track suppression trajectory after a major shock (e.g., a nuclear use event): if suppression persists without active enforcement, it is internalized. If it collapses, it was structural.',
    'If internalized, effective suppression is higher than the structural measure — the constraint carries its own enforcement. If structural, suppression requires active maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the non-proliferation regime').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t1970, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t1985, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t1995, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t2010, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t2020, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2020, 0.33).
narrative_ontology:measurement(npt_withdrawal_threshold_tr_t2025, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2025, 0.35).

% Extraction over time
narrative_ontology:measurement(npt_withdrawal_threshold_be_t1970, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t1985, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1985, 0.25).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t1995, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.5).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t2010, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t2020, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2020, 0.52).
narrative_ontology:measurement(npt_withdrawal_threshold_be_t2025, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(npt_withdrawal_threshold_su_t1970, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t1985, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t1995, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.4).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t2010, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t2020, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2020, 0.43).
narrative_ontology:measurement(npt_withdrawal_threshold_su_t2025, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__withdrawal_threshold_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the NPT Article X withdrawal clause from the broader kernel. The nnws_reading and nws_reading cover the Article VI / non-proliferation bargain. This reading isolates the exit threshold contest. All three readings share the same treaty text but instantiate different constraints with different ε, different stakeholder surfaces, and different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, powerful, 0.7).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, moderate, 0.25).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, institutional, 0.1).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, organized, 0.6).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
