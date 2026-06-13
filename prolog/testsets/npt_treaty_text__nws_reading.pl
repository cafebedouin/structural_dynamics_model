% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Non-Proliferation Binding on NNWS, Disarmament Aspirational (NWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty (NPT), signed 1968, is contested at the
 *   interpretive level. The kernel is Article VI, which commits signatory
 *   states to 'pursue negotiations in good faith on effective measures
 *   relating to cessation of the nuclear arms race at an early date and to
 *   nuclear disarmament.' The NWS reading—instantiated here—interprets this
 *   language as an aspirational goal without enforcement mechanism or binding
 *   timeline. Non-proliferation (Article II, preventing NNWS from acquiring
 *   weapons) is treated as a binding, verifiable obligation. Disarmament
 *   (Article VI) is treated as a long-term aspiration contingent on NWS
 *   consensus, which is unobtainable. This interpretive frame benefits NWS by
 *   preserving their arsenals indefinitely while concentrating verification
 *   authority and transparency obligations on NNWS. The NNWS/NAM reading (a
 *   sibling constraint, separate JSON) interprets Article VI as binding and
 *   views non-proliferation as conditional on NWS compliance. The
 *   withdrawal_threshold_reading (third sibling) contests whether Article X
 *   permits easy exit or requires regime stability. This constraint story
 *   generates ONLY the NWS reading, treating it as a structurally coherent
 *   claim with its own ε, beneficiary/victim structure, and interpretive
 *   commitments.
 *
 * KEY AGENTS:
 *   - nuclear_weapons_states (agenda setter + beneficiary; institutional power; arbitrage exit): interpret disarmament as aspirational, control IAEA budgets and treaty interpretation
 *   - non_nuclear_weapons_states (payer; organized power; constrained exit): submit to inspections, forgo weapons development, receive conditional security guarantees
 *   - IAEA secretariat (agenda setter; institutional power; constrained exit): administers verification focused on horizontal proliferation, leaves vertical arsenals outside scope
 *   - non_aligned movement (payer + excluded; organized power; identity-locked exit): advocates for binding disarmament but structurally sidelined from interpretation
 *   - International Court of Justice (observer; institutional power; analytical exit): has ruled Article VI carries legal weight, but ICJ opinions do not bind treaty interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Non-Proliferation Binding on NNWS, Disarmament Aspirational (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'dc83f332-d94c-4060-bcc2-7314309b4386').
narrative_ontology:cs_kernel_codification('dc83f332-d94c-4060-bcc2-7314309b4386', fixed_text).
narrative_ontology:cs_authority_grounding('dc83f332-d94c-4060-bcc2-7314309b4386', extraction).
narrative_ontology:cs_interpretation_layer_present('dc83f332-d94c-4060-bcc2-7314309b4386').
narrative_ontology:cs_reading_relation('dc83f332-d94c-4060-bcc2-7314309b4386', npt_treaty_text__nnws_reading, forecloses).
narrative_ontology:cs_reading_relation('dc83f332-d94c-4060-bcc2-7314309b4386', npt_treaty_text__withdrawal_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('dc83f332-d94c-4060-bcc2-7314309b4386', foundational, disarmament_aspirational_not_binding).
narrative_ontology:cs_axiom_status(disarmament_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('dc83f332-d94c-4060-bcc2-7314309b4386', disarmament_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('dc83f332-d94c-4060-bcc2-7314309b4386', secondary, verification_asymmetry_necessary).
narrative_ontology:cs_axiom_status(verification_asymmetry_necessary, overridden).
narrative_ontology:cs_axiom_grounding('dc83f332-d94c-4060-bcc2-7314309b4386', verification_asymmetry_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('dc83f332-d94c-4060-bcc2-7314309b4386', equal_security_partnership).
narrative_ontology:cs_drift_state('dc83f332-d94c-4060-bcc2-7314309b4386', post_cold_war_disarmament_expectations, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dc83f332-d94c-4060-bcc2-7314309b4386', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_aligned_movement).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because the constraint extracts asymmetric verification and transparency obligations from NNWS while exempting NWS arsenals; it transfers interpretive control to NWS; and it permits indefinite postponement of disarmament. Suppression is substantial (0.71) because the constraint's persistence depends on actively excluding NAM and threshold-state voices from the interpretation process and blocking amendment mechanisms that would strengthen Article VI. Theater ratio climbs steadily (0.22→0.48) because the NWS reading requires increasing performative justification: disarmament conferences are held with no binding outcomes; NWS pledge periodic reductions (three warheads cut, then arsenals rearm elsewhere); the IAEA expands safeguards inspection capacity while NWS arsenals remain off-limits. The measurement grid is shared across all three metrics at every time point: 1968 (founding, interpretion open), 1985 (Reagan/Thatcher security discourse, interpretation hardens), 2000 (Cold War over, disarmament pressure rises, suppression intensifies), 2010 (NPT Review Conference, failed amendment votes), 2020 (TPNW adopted outside NPT, NWS delegitimation pressure), 2026 (present). The trajectory shows extraction rising early, plateauing after 2010 (the reading is locked in), and theater rising continuously as the regime requires increasing performative activity to sustain the interpretation against growing contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, this is genuine coordination: the NPT prevented arms proliferation, secured extended deterrence for allies, and created IAEA safeguards that verify NNWS compliance. From the NNWS/NAM seats, this is tangled extraction: they accepted non-proliferation asymmetrically (NNWS transparent, NWS opaque) in exchange for a disarmament promise that NWS explicitly interprets as non-binding. The engine computes these divergent classifications from the structural data: NWS derives low/negative extractiveness (beneficiary + low d); NNWS derives high extractiveness (victim + high d). The claimed type is tangled_rope, which the metrics support: genuine coordination function (preventing proliferation spread) layered with asymmetric extraction (verification asymmetry, interpretive control asymmetry, arsenal exemption). The payer and beneficiary seats should compute into different types; the framework detects this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS directionality sits near 0.0 (full beneficiary): they collect from the interpretation (arsenal security, verification asymmetry, interpretive veto), have high institutional power, have arbitrage exit (can withdraw and lose little immediate cost), and operate at civilizational horizon. NNWS directionality sits near 0.85–0.95 (near-total target): they bear verification costs and forgo development capability, have constrained exit (withdrawal means isolation), and operate under generational time pressure. NAM sits near 0.80 (target): organized power, but identity-locked exit (withdrawal is self-delegitimating), generational horizon, and their interests are explicitly sidelined. The engine derives these d values from beneficiary/victim + power + exit + horizon data; no override is needed because the structural asymmetry is clear.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows early mandatrophy indicators. The founding problem (horizontal proliferation prevention) has been substantially solved: fewer than 10 undeclared states have weapons, and proliferation rates have slowed. The treaty's original function is live but weakened. What persists is the interpretive claim (Article VI as aspirational) and the institutional structure (IAEA safeguards). The rising theater_ratio (0.22→0.48) is the diagnostic signal: NWS must perform disarmament commitment (conferences, pledges, symbolic reductions) to maintain legitimacy, but these performances have decoupled from function. The constraint could be reclassified as piton if the theater ratio continues rising and extractiveness plateaus, but at present (2026) extraction is still being actively defended rather than theatrically maintained, so tangled_rope classification holds. The omega variable on 'article_vi_binding_status' directly addresses whether mandatrophy is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Does Article VI constitute a binding legal obligation to pursue nuclear disarmament with a timescale, or is it an aspirational goal without enforcement mechanism?',
    'International Court of Justice would need to deliver a binding judgment on an contentious case (not merely advisory opinion). Alternatively, a treaty amendment adopted by consensus would explicitly define the disarmament obligation''s legal character. Short of that, the question remains contested between the parties.',
    'If Article VI is binding and time-indexed, the NWS reading collapses and the constraint reclassifies to tangled_rope with lower extractiveness (or to snare if NWS simply ignore the binding). If Article VI remains aspirational, the NWS reading holds and extraction persists indefinitely. This is the most consequential omega: it determines whether the constraint is a temporary abuse (snare eventually remedied) or a structurally stable extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'The kernel-level interpretive contest between NWS and NNWS readings.').

omega_variable(
    verification_asymmetry_justification,
    'Is the asymmetry between IAEA inspection of NNWS facilities and non-inspection of NWS arsenals justified as a technical necessity (weapons are harder to verify, require different methods) or as an institutional asymmetry (NWS have veto power and chose to exempt themselves)?',
    'Technical analysis of verification feasibility for weapons-in-stockpile vs. proliferation-prevention inspection. Comparison with verification methods in bilateral arms control treaties (START, New START) that DO verify NWS arsenals. If bilateral treaties prove verification of NWS arsenals is feasible, the asymmetry is revealed as a choice, not a technical necessity.',
    'If asymmetry is technical necessity, the NWS reading gains legitimacy as a pragmatic accommodation. If asymmetry is institutional choice, it appears as pure extraction and strengthens the NNWS reading''s claim that disarmament is non-binding because NWS refused to subject themselves to verification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_asymmetry_justification, empirical, 'Whether verification asymmetry is necessitated by physics or by power.').

omega_variable(
    identity_lock_on_npt_commitment,
    'How much of NNWS compliance with the NPT is a structural constraint (exit would cost them security) vs. an identity lock (they have defined themselves as NPT believers, and withdrawal would dissolve their self-concept)?',
    'Post-withdrawal trajectory analysis: if a state withdraws and quickly acquires weapons with no security consequence, identity lock was primary (the security guarantee was secondary). If a state withdraws and faces immediate isolation/pressure, structural exit cost was primary.',
    'If compliance is structural, the constraint is sustained by rational incentive and could be changed by improving the security guarantee or reducing exit cost. If compliance is identity-locked, the constraint is sustained by belief, and changing terms would require identity-fusion breaking (much harder). This affects the feasibility of reform vs. dissolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_on_npt_commitment, empirical, 'The mechanism sustaining NNWS acceptance of asymmetric verification.').

omega_variable(
    nws_internal_commitment_divergence,
    'Do NWS internally disagree on whether Article VI is binding or aspirational, or is the disagreement purely between NWS (as bloc) and NNWS?',
    'Close-door diplomatic cables (FOIAs, declassification), internal governmental position papers. Do some NWS treat disarmament as binding internally (drafting reduction schedules, legal reasoning) while claiming it is aspirational in public? Or is the claim genuinely unified across the NWS bloc?',
    'If NWS internally treat disarmament as binding but publicly claim it is aspirational (theatrical performance), the theater ratio rises further and mandatrophy accelerates. If NWS genuinely believe their own reading, the constraint is more stable. The finding affects whether the constraint is a lie (snare) or a strategic choice (tangled rope with performance costs).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nws_internal_commitment_divergence, empirical, 'Whether the NWS reading is unified across the bloc or theatrically maintained.').

omega_variable(
    iaea_budget_allocation_choice,
    'Is IAEA''s concentration of safeguards resources on horizontal proliferation (NNWS detection) vs. vertical arsenal monitoring (NWS verification) a technical priority (horizontal proliferation is the greater threat) or an institutional capture (NWS control the IAEA board and prefer resources focused on NNWS)?',
    'Counterfactual analysis: if IAEA had the same budget but could allocate freely, what fraction would it assign to NWS arsenal verification vs. NNWS safeguards? Expert elicitation from IAEA technical staff (would they detect violations if tasked?). Comparison with IAEA''s historical efforts to inspect NWS facilities (Iraq aftermath inspections) to assess technical capacity.',
    'If the focus is a technical priority, the verification asymmetry is justified and extraction is lower than claimed. If the focus is institutional capture, the asymmetry is a mechanism for sustaining the extraction and the theater ratio rises (performative safeguards that miss the real threat: NWS arsenals).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_budget_allocation_choice, empirical, 'Whether IAEA budget allocation reflects technical threat assessment or institutional power dynamics.').

omega_variable(
    reading_stability_under_npt_amendments,
    'If a future NPT amendment explicitly added a binding disarmament timeline with verification mechanisms, could NWS ratify the amendment while maintaining the NWS reading, or would ratification constitute acceptance of the NNWS reading?',
    'Treaty amendment process: if amendment is adopted by consensus (or supermajority), the NWS interpretive claim becomes formally overridden. If NWS block amendment, the NWS reading persists by veto. This tests whether the reading is defended by argument or by institutional power.',
    'If NWS can maintain the reading after a binding amendment (through clever implementing clauses or reservations), the constraint is more extractive than coded—it is snare rather than tangled rope because no coordination function is genuinely traded. If amendment would dissolve the reading, the reading''s stability depends on veto power, confirming the suppression component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_stability_under_npt_amendments, conceptual, 'Whether the NWS reading can survive a binding amendment or requires veto power to persist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nws_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nws_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.44).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nws_reading, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_text__nws_reading, theater_ratio, 2026, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nws_reading, base_extractiveness, 1968, 0.55).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nws_reading, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nws_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_text__nws_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__nws_reading, suppression_requirement, 1968, 0.48).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nws_reading, suppression_requirement, 1985, 0.56).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nws_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_text__nws_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nws_reading, 0.18).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT kernel (Article VI disarmament language). The NNWS reading (npt_treaty_text__nnws_reading) interprets the same language as binding and time-indexed, treating non-proliferation as conditional on NWS disarmament progress. The withdrawal_threshold reading (npt_treaty_text__withdrawal_threshold_reading) addresses Article X exit mechanisms separately. The three readings have different ε values and beneficiary/victim structures because they instantiate different constraints—not different viewpoints on the same constraint. The NWS reading has the highest extractiveness because interpretive control of 'aspirational' permits unlimited arsenal preservation. The NNWS reading has lower extractiveness because binding disarmament would constrain NWS. The withdrawal_threshold reading is orthogonal—it trades off constraint strength (exit cost) against state sovereignty. Do not merge these into one constraint; do not average over them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, organized, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
