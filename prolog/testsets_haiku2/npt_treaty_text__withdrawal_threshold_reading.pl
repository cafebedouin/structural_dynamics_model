% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Interpretation (High-Threshold Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   The NPT's Article X permits withdrawal 'with three months' notice' if
 *   extraordinary events jeopardize a signatory's supreme interests. This
 *   constraint is the high-threshold reading of that provision: the reading
 *   that interprets 'extraordinary events' as requiring existential or
 *   catastrophic security threats, not routine strategic recalculation. This
 *   reading prioritizes regime stability by making withdrawal rhetorically
 *   costly and ambiguous. The competing reading (the low-threshold reading,
 *   not this constraint) would treat 'extraordinary events' as referring to
 *   any material shift in security circumstances, making withdrawal a
 *   credible threat tool. This constraint IS the high-threshold
 *   instantiation; it is not a compromise between the two readings. The North
 *   Korea precedent (2003 withdrawal) sits in the ambiguous space: North
 *   Korea invoked Article X but the NWS did not prevent withdrawal nor did
 *   they accept the withdrawal narrative as legitimate, leaving the
 *   threshold's enforceability permanently uncertain.
 *
 * KEY AGENTS:
 *   - threshold_states (Iran, Japan, South Korea, Turkey): benefit from exit option credibility; constraint preserves their strategic position
 *   - existing_nws_regime_stability (institutional interest): benefits from high threshold; constraint protects monopoly
 *   - sovereignty_preserving_nnws (Germany, Canada, Brazil): pay the cost of constrained exit; identity-locked into non-proliferation identity
 *   - proliferation_crisis_responders (IAEA, UN Security Council): bear ambiguity cost; must navigate post-hoc judgment
 *   - nws_enforcement_coalition (P5 states): agenda-setters; maintain high threshold through practice and selective non-enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.62).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Interpretation (High-Threshold Reading)").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, 'aefe27a6-12ed-452f-af51-1ed4a2a4e5c2').
narrative_ontology:cs_kernel_codification('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', fixed_text).
narrative_ontology:cs_authority_grounding('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', extraction).
narrative_ontology:cs_interpretation_layer_present('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2').
narrative_ontology:cs_reading_relation('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_axiom('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', foundational, regime_stability_priority_over_exit_clarity).
narrative_ontology:cs_axiom_status(regime_stability_priority_over_exit_clarity, holdable).
narrative_ontology:cs_axiom_grounding('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', regime_stability_priority_over_exit_clarity, instrumental).
narrative_ontology:cs_axiom('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', foundational, extraordinary_events_requires_existential_threat).
narrative_ontology:cs_axiom_status(extraordinary_events_requires_existential_threat, holdable).
narrative_ontology:cs_axiom_grounding('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', extraordinary_events_requires_existential_threat, conventional).
narrative_ontology:cs_reference_frame('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', npt_as_binding_non_proliferation_regime).
narrative_ontology:cs_drift_state('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', post_north_korea_withdrawal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aefe27a6-12ed-452f-af51-1ed4a2a4e5c2', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, existing_nws_regime_stability).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, sovereignty_preserving_nnws).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, proliferation_crisis_responders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with weapons-usable nuclear materials and technological capacity to weaponize (Iran, Japan, South Korea, Turkey) benefit from a high withdrawal threshold because it credibly preserves their exit option: they can signal 'we retain the right to withdraw if circumstances change' without triggering immediate coercive response. A low threshold would make withdrawal politically routine, eliminating the strategic signaling value of restraint. They coordinate around maintaining the ambiguity of the threshold itself as a safety valve.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states, beneficiary,
    powerful, generational, constrained, global).

% The institutional interest in preserving the existing five-state NWS monopoly benefits from a high withdrawal threshold because it prevents rapid exit by threshold states. The higher the barrier to withdrawal, the more binding the non-proliferation commitment appears, and the lower the pressure to accommodate new entrants or renegotiate the treaty's asymmetry.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, existing_nws_regime_stability, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__withdrawal_threshold_reading, existing_nws_regime_stability).

% States that accept non-proliferation as a core commitment (Germany, Canada, Brazil) but view withdrawal as a fundamental sovereignty right. They bear the cost of a high threshold because it constrains their ability to exit if circumstances radically change (existential security threat, treaty violation by others). Their identity as law-abiding treaty partners makes identity-locked exit operative: even if withdrawal becomes technically possible, the reputational cost and self-concept alignment prevent it. A high threshold entrenches this lock.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, sovereignty_preserving_nnws, payer,
    moderate, biographical, identity_locked, national).

% International institutions (IAEA, UN Security Council) tasked with responding to proliferation crises or treaty violations. They bear the cost of a high threshold because it creates ambiguity about what constitutes a valid withdrawal trigger, forcing post-hoc judgment calls on whether 'extraordinary events' justify exit. A low threshold would create clearer rules; a high threshold delegates the judgment to the withdrawing state, shifting authority and creating enforcement burden on responders.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, proliferation_crisis_responders, payer,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, proliferation_crisis_responders, observer).

% North Korea's 2003 withdrawal invoked Article X's 'extraordinary events' language and declared the NPT no longer binding, claiming security threats justified exit. The precedent is ambiguous: it demonstrates that withdrawal is technically possible under a high-threshold reading, but also that the threshold provides no enforceable constraint on the withdrawing state's unilateral interpretation. Responders must navigate whether to accept the withdrawal claim or contest it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, north_korea_precedent, observer,
    powerful, biographical, analytical, global).

% The P5 states collectively maintain the high-threshold reading as institutional practice through non-response to marginal withdrawal claims (tolerating North Korea's withdrawal without collective action) and through selective enforcement (resisting Iran withdrawal narratives while accepting North Korea's precedent). This ambivalent enforcement is what keeps the threshold operative as a structural constraint without legal clarity.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nws_enforcement_coalition, agenda_setter,
    institutional, generational, arbitrage, global).

% Civil society, NAM bloc states, and disarmament-focused delegations would argue for a low threshold (easy exit pathway becomes pressure on NWS to honor Article VI disarmament) but are structurally absent from the treaty's formal amendment and interpretation apparatus. They lack standing to formally contest the high-threshold reading.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nnws_disarmament_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, nws_enforcement_coalition).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the non-proliferation regime's stability by making withdrawal costly and ambiguous: states that might leave face reputational, strategic, and institutional friction. The high threshold is justified as protecting the regime from defection cascades if any state could exit easily.
% TRANSFER_FUNCTION: Moves strategic restraint from threshold states (who forgo weapons programs) to regime stability (which concentrates NWS authority). In exchange, threshold states retain the perception of an exit option, which satisfies domestic audiences and keeps restraint politically sustainable.
% ABSENT_VOICES: Non-aligned and disarmament-focused states (NAM bloc, Austria, Brazil) would argue the high threshold protects NWS interests at NNWS expense and that a low threshold would create pressure for genuine Article VI disarmament. They are excluded from the formal treaty body interpreting Article X.
% DISAPPEARANCE_RATIONALE: If this high-threshold reading were replaced by a low-threshold reading (easy withdrawal = no reputational barrier, clear exit right), threshold states would recalculate their commitment, disarmament advocates would leverage withdrawal threats as leverage on NWS, and the regime's apparent stability would fragment into explicit renegotiation. The constraint's disappearance would force the treaty's ambiguity into open contestation.
% FOUNDING_PROBLEM: The NPT's foundational bargain (1968) is asymmetric: NWS retain weapons, NNWS renounce them. This bargain is only politically sustainable if NNWS believe they retain some exit right if the NWS fails to disarm. The high-threshold withdrawal reading allows NWS to claim the exit right is conditional and costly (protecting regime stability) while NNWS can claim the exit right is real (preserving sovereignty). Both interpretations coexist because the treaty text leaves 'extraordinary events' undefined.
% FOUNDING_PROBLEM_CORROBORATION: The NWS emphasize regime stability and the necessity of a binding, difficult-to-exit commitment. The NNWS (especially Iran, Iraq historically, potential future threshold states) emphasize sovereignty and the non-negotiability of the right to withdraw. International law scholars are split: some cite the treaty's object-and-purpose (non-proliferation regime stability) as supporting the high threshold; others cite state sovereignty principles and the treaty's express language 'with three months' notice' as supporting easy exit. No consensus exists outside the divergent institutional positions.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.62 at endpoint) because the constraint is genuinely dyadic: it does coordinate the regime's stability (a real function) but simultaneously extracts from NNWS by locking them into an ambiguous and asymmetric commitment. The measurement series show extractiveness rising over the interval (0.48 → 0.62) as Iran's pursuit of nuclear capacity and repeated withdrawal threats gradually shift the interpretation's de facto burden: threshold states must repeatedly reaffirm restraint under heightened scrutiny, increasing the extraction. Suppression (0.58) is substantial but lower than extractiveness because the constraint's force is not primarily coercive—it operates through reputational cost and ambiguity. The North Korea precedent explicitly permits withdrawal under the treaty's own terms, which limits suppression compared to a true legal barrier. Theater ratio (0.41) is moderate: the formal withdrawal-review apparatus exists and functions (not purely theatrical), but a growing share of the constraint's actual work is diplomatic signaling and ambiguity management rather than enforcement of clear rules. The shared time grid spans the interval [0, 56] (post-2003 North Korea withdrawal to contemporary 2026 Iranian escalation). The cyclical pattern of crisis → reassurance → crisis visible in measurement data reflects the constraint's exposure to threshold-state decisions and proliferation crises.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS enforcement coalition's seat, this is a rope: it genuinely coordinates regime stability and they maintain it through institutional practice. From threshold states' seats, it is a tangled rope: they coordinate on non-proliferation but extract enormous strategic value from the exit option's credibility—they benefit from the ambiguity, not from clarity. From NNWS disarmament advocates' seats (excluded), it is a snare: it locks non-proliferation commitment in place while NWS escape disarmament obligations through the same 'extraordinary events' language. The engine computes these divergences from the structural data: the NWS have arbitrage-grade exit (they can reinterpret the threshold politically), threshold states have constrained exit (they cannot exit without reputational catastrophe), and disarmament advocates are excluded from the formal apparatus. The type divergence (rope → tangled rope → snare across seats) is precisely what the constraint structure produces.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: threshold_states (powerful, constrained exit, but benefit from the strategic value of the exit option itself) and existing_nws_regime_stability (institutional, arbitrage exit—the regime can be defended or abandoned depending on institutional incentives). Victims: sovereignty_preserving_nnws (moderate power, identity-locked exit—they cannot credibly withdraw without self-concept dissolution) and proliferation_crisis_responders (institutional power, mobile exit—they can change their enforcement posture, but the constraint forces them into ambiguous judgment calls). The directionality divergence is structural: beneficiaries have exit options that amplify their benefit (threshold states' exit option is valuable precisely because withdrawal is costly), while victims are locked in or burdened by the ambiguity. No directionality overrides needed—the structural derivation captures the true positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the NPT's asymmetry: NNWS renounce weapons, NWS retain them and promise eventual disarmament. This asymmetry is only politically sustainable if NNWS believe they retain an exit right. The high-threshold reading preserves this belief while simultaneously making exit operationally difficult, satisfying both sides of the foundational bargain. The mandatrophy question is whether the withdrawal threshold's ambiguity is still solving the founding problem or has become a zombie constraint—a piece of the regime that persists because ambiguity itself is now beneficial to all the institutional actors, regardless of whether it serves the bargain's original purpose. The evidence points to partial mandatrophy: threshold states now invoke the exit option as a strategic signaling tool (Iran's repeated withdrawal rhetoric), not as a genuine last-resort mechanism. The constraint has drifted from 'preserving the appearance of an exit' (original function) to 'maintaining strategic ambiguity about withdrawal' (current function). This drift is not complete enough to classify as piton because the constraint still serves a real coordination function (it does keep the regime nominally stable), but the original founding problem (sustaining NNWS commitment to the bargain) is contested—some NNWS now view the asymmetry as irredeemable and the threshold as a tool of NWS power rather than as a safety valve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_events_threshold_ambiguity,
    'Does ''extraordinary events'' in Article X mean existential/catastrophic threats only (high threshold), or any material shift in security circumstances (low threshold)?',
    'Formal treaty amendment clarifying the term; advisory opinion from International Court of Justice; consensus interpretation by treaty amendment conference.',
    'A clarified high threshold would shift classification toward rope (regime-stabilizing coordination); a clarified low threshold would shift toward snare (easy exit reduces credibility of withdrawal threat, increasing regime''s binding force through legal clarity rather than ambiguity).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_events_threshold_ambiguity, conceptual, 'The treaty text is intentionally ambiguous; the threshold cannot be resolved without amending the treaty or achieving consensus interpretation, which the NWS coalition has avoided.').

omega_variable(
    north_korea_precedent_normativity,
    'Does the North Korea 2003 withdrawal establish a practice that crystallizes the high-threshold reading as binding state practice, or does it remain a contested deviation?',
    'Empirical: count how many subsequent withdrawal claims states make and how they are treated by the institutional apparatus. If states begin treating withdrawal as a low-threshold right, the precedent will shift the interpretation downward.',
    'If North Korea''s withdrawal is accepted as precedent-setting, the high threshold erodes and the constraint shifts toward a more permissive regime. If it remains contested, the threshold is preserved through institutional non-response.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_normativity, empirical, 'Precedent operates through repeat cases; the ambiguity persists because no second withdrawal has been definitively upheld or rejected.').

omega_variable(
    threshold_state_strategic_calculation,
    'Are threshold states'' periodic withdrawal rhetoric and program developments genuine expressions of the exit option, or strategic signaling using the threshold''s ambiguity?',
    'Disclosed state decision-making; comparison of withdrawal rhetoric to actual proliferation breakout timelines; behavioral shift if the threshold is suddenly clarified to low (do states accelerate programs?).',
    'If genuinely expressing exit threats, the threshold is failing to constrain—states are already positioned at the edge of withdrawal. If strategic signaling, the threshold is successfully maintaining the ambiguity that preserves restraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_state_strategic_calculation, empirical, 'The functional question beneath the legal one: does the ambiguous threshold work as a commitment device, or has it become a tool threshold states use to extract concessions?').

omega_variable(
    reading_dependent_epsilon,
    'This constraint instantiates the high-threshold reading. A sibling low-threshold reading would have a different ε and a different classification. How does the divergence in ε across readings affect mandatrophy and regime stability assessment?',
    'Compare the three readings'' metrics when all three constraints are authored (omega_c: framing under-determination). The constraint family''s internal divergence is the signal that the kernel''s interpretation is contested.',
    'High-threshold reading (this one) preserves regime stability but locks NNWS into ambiguity; low-threshold reading would destabilize the regime but empower NNWS. The treaty''s foundational problem (asymmetry sustainability) cannot be solved by either reading alone—it requires both to coexist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_dependent_epsilon, conceptual, 'This is a committer-frame omega: the irreducible ambiguity is that the kernel text cannot be read in a way that satisfies both regime stability AND NNWS sovereignty simultaneously.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t8, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement_basis(npt__tr_t8, observed).
narrative_ontology:measurement(npt__tr_t16, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement_basis(npt__tr_t16, observed).
narrative_ontology:measurement(npt__tr_t24, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(npt__tr_t24, observed).
narrative_ontology:measurement(npt__tr_t32, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement_basis(npt__tr_t32, observed).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(npt__tr_t40, observed).
narrative_ontology:measurement(npt__tr_t48, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 48, 0.41).
narrative_ontology:measurement_basis(npt__tr_t48, observed).
narrative_ontology:measurement(npt__tr_t56, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 56, 0.41).
narrative_ontology:measurement_basis(npt__tr_t56, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t8, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(npt__be_t8, observed).
narrative_ontology:measurement(npt__be_t16, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(npt__be_t16, observed).
narrative_ontology:measurement(npt__be_t24, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(npt__be_t24, observed).
narrative_ontology:measurement(npt__be_t32, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 32, 0.61).
narrative_ontology:measurement_basis(npt__be_t32, observed).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(npt__be_t40, observed).
narrative_ontology:measurement(npt__be_t48, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 48, 0.62).
narrative_ontology:measurement_basis(npt__be_t48, observed).
narrative_ontology:measurement(npt__be_t56, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 56, 0.62).
narrative_ontology:measurement_basis(npt__be_t56, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t8, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(npt__su_t8, observed).
narrative_ontology:measurement(npt__su_t16, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement_basis(npt__su_t16, observed).
narrative_ontology:measurement(npt__su_t24, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement_basis(npt__su_t24, observed).
narrative_ontology:measurement(npt__su_t32, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement_basis(npt__su_t32, observed).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement_basis(npt__su_t40, observed).
narrative_ontology:measurement(npt__su_t48, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 48, 0.58).
narrative_ontology:measurement_basis(npt__su_t48, observed).
narrative_ontology:measurement(npt__su_t56, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 56, 0.58).
narrative_ontology:measurement_basis(npt__su_t56, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, iran_nuclear_program_commitment).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, north_korea_npt_precedent).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article X kernel. The kernel is the treaty text's ambiguous 'extraordinary events' provision. The three readings (nws_reading, nnws_reading, withdrawal_threshold_reading) have different epsilon values and different beneficiary/victim structures because they instantiate different interpretations of what counts as a valid withdrawal trigger. They are linked as a constraint family: none can be understood in isolation. The high-threshold reading (this constraint) prioritizes regime stability; the low-threshold reading would prioritize NNWS sovereignty; the disarmament-priority reading would make withdrawal a pressure tool on NWS compliance. Each is a structurally distinct constraint with its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
