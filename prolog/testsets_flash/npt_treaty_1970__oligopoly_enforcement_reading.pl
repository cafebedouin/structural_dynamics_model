% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Treaty (1970) - Oligopoly Enforcement Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint represents the NPT (Treaty on the Non-Proliferation of
 *   Nuclear Weapons) as primarily an instrument for enforcing a nuclear
 *   oligopoly, where Articles I and II (preventing horizontal proliferation)
 *   are binding obligations, while Article VI (disarmament by Nuclear Weapon
 *   States) is treated as aspirational and contingent. This reading
 *   emphasizes the enforcement asymmetry, with high inspection burdens on
 *   Non-Nuclear Weapon States (NNWS) and minimal accountability for Nuclear
 *   Weapon States (NWS). Threshold states are considered victims, denied a
 *   deterrent capability. The P5 (NWS) are the primary beneficiaries of this
 *   status hierarchy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.65).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.75).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Treaty (1970) - Oligopoly Enforcement Reading").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '1bcb3497-ff5e-4c0c-a9e6-43f614270104').
narrative_ontology:cs_kernel_codification('1bcb3497-ff5e-4c0c-a9e6-43f614270104', fixed_text).
narrative_ontology:cs_authority_grounding('1bcb3497-ff5e-4c0c-a9e6-43f614270104', extraction).
narrative_ontology:cs_interpretation_layer_present('1bcb3497-ff5e-4c0c-a9e6-43f614270104').
narrative_ontology:cs_reading_relation('1bcb3497-ff5e-4c0c-a9e6-43f614270104', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('1bcb3497-ff5e-4c0c-a9e6-43f614270104', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1bcb3497-ff5e-4c0c-a9e6-43f614270104', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('1bcb3497-ff5e-4c0c-a9e6-43f614270104', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('1bcb3497-ff5e-4c0c-a9e6-43f614270104', foundational, nws_nuclear_arsenals_are_legitimate_deterrent).
narrative_ontology:cs_axiom_status(nws_nuclear_arsenals_are_legitimate_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('1bcb3497-ff5e-4c0c-a9e6-43f614270104', nws_nuclear_arsenals_are_legitimate_deterrent, conventional).
narrative_ontology:cs_reference_frame('1bcb3497-ff5e-4c0c-a9e6-43f614270104', npt_as_horizontal_nonproliferation_regime).
narrative_ontology:cs_drift_state('1bcb3497-ff5e-4c0c-a9e6-43f614270104', contemporary_review_conferences, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bcb3497-ff5e-4c0c-a9e6-43f614270104', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear weapon states (US, UK, France, Russia, China) who drafted the NPT. They benefit from the non-proliferation regime by maintaining their nuclear monopoly and preventing new states from acquiring nuclear weapons, while facing no binding obligation to disarm themselves. They enforce Articles I and II through diplomatic pressure, sanctions, and military threats.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% States that have foresworn nuclear weapons under the NPT. They bear the burden of IAEA inspections and verification, limiting their sovereign control over their nuclear programs, while receiving only aspirational commitments for disarmament from the NWS. Their exit options are constrained by the high political and economic costs of withdrawal.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws, payer,
    organized, generational, constrained, global).

% Non-nuclear weapon states with advanced nuclear capabilities or security concerns that might lead them to develop nuclear weapons. They are denied a deterrent capability that NWS possess, placing them at a strategic disadvantage. Their 'identity lock' comes from the international norm against proliferation and the severe consequences of violating it.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    moderate, biographical, identity_locked, regional).

% The International Atomic Energy Agency, which implements safeguards and verification measures on NNWS. Its mandate and funding are directly tied to the NPT regime, making it a beneficiary of the enforcement structure, even if it operates with technical neutrality. Its power is derived from the NPT's enforcement mechanisms.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea, beneficiary,
    institutional, generational, constrained, global).

% Academics, think tanks, and policy experts who study nuclear proliferation and the NPT. They analyze the treaty's effectiveness, its asymmetries, and its long-term implications for international security. Their role is to provide independent assessment and critique.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, global_security_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the horizontal proliferation of nuclear weapons by establishing a norm against new nuclear weapon states and a verification regime for non-nuclear weapon states.
% TRANSFER_FUNCTION: Transfers the burden of verification and the strategic disadvantage of non-possession from nuclear weapon states to non-nuclear weapon states, in exchange for a contingent promise of disarmament and access to peaceful nuclear technology.
% ABSENT_VOICES: States that have never joined the NPT (e.g., India, Pakistan, Israel) or have withdrawn (e.g., North Korea) are absent from the internal discourse, as are those who advocate for a more equitable, disarmament-focused regime. They would argue that the NPT legitimizes a dangerous nuclear oligopoly.
% DISAPPEARANCE_RATIONALE: If the NPT vanished overnight, the international security landscape would fundamentally rearrange. Many NNWS would likely pursue nuclear weapons, leading to rapid horizontal proliferation, increased regional instability, and a higher risk of nuclear conflict. The IAEA's primary function would cease to exist.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the fear of widespread nuclear proliferation following the Cuban Missile Crisis, leading to a desire to limit the number of states possessing nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: The NWS and the IAEA consistently attest that the threat of proliferation remains live, citing ongoing challenges from threshold states and the need for continued vigilance. Many NNWS and independent analysts, while acknowledging the initial problem, argue that the NPT's current structure exacerbates the problem by entrenching NWS privilege, a view supported by historical records of NPT review conferences and academic critiques.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is driven by the asymmetric burden-sharing: NNWS bear the costs of verification and foregone nuclear options, while NWS retain their arsenals. Suppression (0.75) is high due to the severe diplomatic, economic, and potential military consequences for states attempting to 'break out' of the regime. The theater ratio (0.4) reflects the performative aspect of NWS engaging in disarmament talks without concrete, time-bound commitments, while the core function of horizontal non-proliferation remains active. The increasing trend in extractiveness and suppression over time reflects the hardening of the non-proliferation regime against NNWS, without a corresponding increase in NWS disarmament.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS perspective, the NPT is a successful coordination mechanism preventing global catastrophe. From the NNWS and threshold states' perspective, it is an extractive regime that perpetuates an unjust and dangerous nuclear oligopoly. The engine's per-seat classification will reflect this divergence, with NWS seats computing as Rope/Scaffold and NNWS seats computing as Snare/Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nuclear Weapon States (P5) are clear beneficiaries and agenda-setters (d near 0.0), as the constraint preserves their strategic advantage. The IAEA, while a technical body, also benefits from the regime's perpetuation (d near 0.15). Non-Nuclear Weapon States (NNWS) and especially threshold states are targets (d near 1.0), bearing the costs of verification and foregone strategic options. Global security analysts are observers (d=0.5).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Is Article VI (disarmament by NWS) a binding legal obligation with temporal urgency, or an aspirational goal contingent on the security environment?',
    'International Court of Justice ruling on NWS compliance with Article VI, or a new treaty explicitly setting time-bound disarmament targets.',
    'If binding and urgent, the NPT''s extractiveness from NNWS would be re-evaluated downward, and the NWS''s position would shift from beneficiary to non-compliant target. This would reclassify the constraint closer to a Snare for NWS.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'Ambiguity regarding the legal force and timeline of NWS disarmament obligations.').

omega_variable(
    proliferation_risk_attribution,
    'Is the primary risk of proliferation driven by horizontal spread (NNWS acquiring weapons) or vertical proliferation (NWS modernizing/expanding arsenals)?',
    'Empirical analysis of historical proliferation drivers and ''near misses,'' and expert consensus on future risk scenarios.',
    'If vertical proliferation is the primary driver, the NPT''s focus on NNWS becomes a misdirection, increasing the perceived theater ratio and extractiveness from NNWS, as the constraint addresses the ''wrong'' problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_risk_attribution, empirical, 'Whether the NPT addresses the most salient proliferation risk.').

omega_variable(
    oligopoly_vs_stability,
    'Does the NPT''s asymmetric structure primarily serve to maintain a nuclear oligopoly, or is it a necessary (if imperfect) mechanism for global stability?',
    'Counterfactual analysis of a world without the NPT, or a ''grand bargain'' scenario where NWS disarm in exchange for verifiable NNWS non-proliferation.',
    'If primarily oligopoly, the constraint is more extractive and snare-like. If primarily stability, the coordination function is stronger, pushing it closer to a Rope, despite asymmetries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_stability, preference, 'Framing of the NPT''s core function: power maintenance vs. global good.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Treaty (1970). Other readings include 'reciprocal_disarmament_reading' and 'withdrawal_sovereignty_reading', which emphasize different articles and interpretations of the treaty's core bargain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
