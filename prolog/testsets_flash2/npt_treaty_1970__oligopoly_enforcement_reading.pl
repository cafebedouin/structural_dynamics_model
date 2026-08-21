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
    narrative_ontology:affects_constraint/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT 1970 Treaty (Oligopoly Enforcement Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story represents the 'oligopoly enforcement' reading of
 *   the 1970 Nuclear Non-Proliferation Treaty (NPT). In this reading,
 *   Articles I and II, which prevent horizontal proliferation (NNWS acquiring
 *   nuclear weapons), are treated as the primary, binding obligations.
 *   Article VI, which calls for nuclear disarmament by the Nuclear Weapon
 *   States (NWS), is interpreted as a contingent and aspirational goal,
 *   lacking the same enforcement mechanisms or urgency. This reading
 *   highlights the asymmetry of the NPT regime, where the P5 states benefit
 *   from maintaining their nuclear arsenals while imposing strict
 *   nonproliferation requirements on others. The claimed type is
 *   'tangled_rope' because it genuinely coordinates nonproliferation but does
 *   so with significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.78).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT 1970 Treaty (Oligopoly Enforcement Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'a6287e56-5248-4417-b482-f85d8fead9e6').
narrative_ontology:cs_kernel_codification('a6287e56-5248-4417-b482-f85d8fead9e6', fixed_text).
narrative_ontology:cs_authority_grounding('a6287e56-5248-4417-b482-f85d8fead9e6', extraction).
narrative_ontology:cs_interpretation_layer_present('a6287e56-5248-4417-b482-f85d8fead9e6').
narrative_ontology:cs_reading_relation('a6287e56-5248-4417-b482-f85d8fead9e6', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6287e56-5248-4417-b482-f85d8fead9e6', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('a6287e56-5248-4417-b482-f85d8fead9e6', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('a6287e56-5248-4417-b482-f85d8fead9e6', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('a6287e56-5248-4417-b482-f85d8fead9e6', foundational, nws_arsenals_are_legitimate_deterrent).
narrative_ontology:cs_axiom_status(nws_arsenals_are_legitimate_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('a6287e56-5248-4417-b482-f85d8fead9e6', nws_arsenals_are_legitimate_deterrent, conventional).
narrative_ontology:cs_reference_frame('a6287e56-5248-4417-b482-f85d8fead9e6', p5_nuclear_oligopoly_stability).
narrative_ontology:cs_drift_state('a6287e56-5248-4417-b482-f85d8fead9e6', contemporary_global_security_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6287e56-5248-4417-b482-f85d8fead9e6', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear weapon states (P5) who drafted the NPT. They benefit from the treaty's primary focus on preventing horizontal proliferation (Articles I-II) while their own disarmament obligations (Article VI) remain aspirational and largely unenforced. They maintain their nuclear arsenals and veto power in the UN Security Council, effectively enforcing the nonproliferation regime on others.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% States that have renounced nuclear weapons under the NPT. They bear the burden of IAEA inspections and safeguards, limiting their sovereign nuclear programs, while perceiving a lack of reciprocal disarmament from the P5. Their exit options are constrained by international pressure and potential sanctions.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws, payer,
    organized, biographical, constrained, global).

% States with the technical capacity to develop nuclear weapons but which have not done so, or have done so outside the NPT framework. They are denied the deterrent capability that the P5 retain, creating a security asymmetry. Their 'identity lock' stems from the international norm against proliferation and the severe consequences of overt weaponization.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    powerful, biographical, identity_locked, regional).

% The international body responsible for verifying NNWS compliance with NPT safeguards. It enforces the horizontal nonproliferation aspects of the treaty, conducting inspections and reporting violations, but has no mandate to enforce Article VI disarmament on the P5. Its funding and mandate are subject to P5 influence.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, international_atomic_energy_agency_iaea, agenda_setter,
    institutional, generational, constrained, global).

% Advocate for universal nuclear disarmament and the full implementation of Article VI. They are largely excluded from the core decision-making processes of the NPT regime, despite their moral and political pressure. Their 'constrained' exit reflects the difficulty of shifting entrenched state policies.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, global_civil_society_disarmament_advocates, excluded,
    moderate, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the horizontal proliferation of nuclear weapons by establishing a global norm and verification regime, thereby reducing the risk of nuclear war among non-nuclear states.
% TRANSFER_FUNCTION: Transfers the burden of verification and the renunciation of nuclear weapons from the P5 to the NNWS, in exchange for a contingent promise of disarmament and peaceful nuclear technology access.
% ABSENT_VOICES: Global civil society and disarmament advocates, as well as states that feel their security is compromised by the P5's continued nuclear arsenals, are largely excluded from shaping the NPT's enforcement priorities. They would argue for a more equitable and binding disarmament framework.
% DISAPPEARANCE_RATIONALE: If the NPT vanished overnight, the global nuclear landscape would rapidly destabilize. Many NNWS would likely pursue nuclear weapons programs, leading to a cascade of proliferation, increased regional conflicts, and a higher risk of nuclear war. The P5's security would also be fundamentally altered.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent a world with dozens of nuclear-armed states, while preserving the security interests of existing nuclear powers.
% FOUNDING_PROBLEM_CORROBORATION: The P5 states consistently attest that the founding problem of horizontal proliferation remains live and critical. Many NNWS and international observers corroborate the continued threat of proliferation, but contest the P5's framing of Article VI as merely aspirational, arguing that the vertical proliferation by NWS exacerbates the problem.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.78) because NNWS bear significant costs (inspections, renunciation of a strategic deterrent) without receiving the promised reciprocal disarmament. Suppression (0.85) is very high, reflecting the severe international penalties (sanctions, military action) for states that violate nonproliferation norms. Theater ratio (0.45) is moderate and rising, as the P5's rhetorical commitment to Article VI increasingly diverges from their actual disarmament efforts, making the 'bargain' appear more performative. The metrics reflect the NPT's operation from the perspective of NNWS and threshold states, who experience it as a highly extractive and suppressive regime.
 *
 * PERSPECTIVAL GAP:
 *   The P5 states would likely classify the NPT as a 'rope' or even a 'mountain' (a natural necessity for global stability), emphasizing its coordination function. However, from the perspective of NNWS and threshold states, the same structure operates as a 'snare' or 'tangled_rope' due to the enforcement asymmetry and the perceived lack of good faith on Article VI. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nuclear Weapon States (P5) are clear beneficiaries and agenda-setters (d near 0.0), as they retain their arsenals and control the enforcement mechanisms. Non-Nuclear Weapon States (NNWS) and threshold states are the primary targets/victims (d near 1.0), bearing the costs of nonproliferation without the reciprocal benefits. The IAEA, while an enforcer, operates under the mandate largely shaped by the P5. Global civil society is excluded, experiencing the constraint as a barrier to their disarmament goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the NPT as a pure 'rope' by highlighting the asymmetric enforcement and the atrophied commitment to Article VI. While the core mandate of horizontal nonproliferation remains live, the 'bargain' aspect of the treaty (disarmament for nonproliferation) has suffered mandatrophy, transforming it into a more extractive arrangement. The high extractiveness and suppression, coupled with the rising theater ratio, indicate that the constraint's function has drifted from a balanced coordination mechanism to one that primarily serves the interests of the P5.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Is Article VI of the NPT a binding legal obligation with temporal urgency, or a contingent, aspirational goal?',
    'International Court of Justice advisory opinion on the legal status and enforceability of Article VI, or a new treaty explicitly setting disarmament timelines.',
    'If binding and urgent, the NPT''s extractiveness on NNWS would decrease, and the P5''s directionality would shift towards being targets of disarmament, potentially reclassifying the constraint closer to a ''rope'' or ''scaffold''. If aspirational, the current ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'Ambiguity regarding the legal force and urgency of NWS disarmament obligations.').

omega_variable(
    security_dilemma_justification,
    'To what extent does the P5''s retention of nuclear weapons genuinely serve global stability and deterrence, versus maintaining a strategic advantage and status hierarchy?',
    'Independent, non-state-aligned strategic analyses of global security without nuclear weapons, or a ''no-first-use'' and ''minimum deterrence'' policy shift by the P5.',
    'If primarily for strategic advantage, the NPT''s extractiveness is higher, reinforcing the ''tangled_rope'' or ''snare'' classification. If genuinely for global stability, the coordination function is stronger, potentially lowering extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_justification, empirical, 'Whether NWS arsenals are primarily for global stability or status/advantage.').

omega_variable(
    horizontal_vs_vertical_proliferation_threat,
    'Is horizontal proliferation (more states acquiring nuclear weapons) a greater threat to global security than vertical proliferation (existing NWS modernizing and expanding their arsenals)?',
    'Comprehensive risk assessment models that integrate both horizontal and vertical proliferation dynamics, accounting for accident risk, regional instability, and arms race dynamics.',
    'If vertical proliferation is deemed a greater or equal threat, the NPT''s current asymmetric enforcement (focused on horizontal) would be seen as less effective and more extractive, potentially pushing the classification towards ''snare''. If horizontal remains the dominant threat, the current ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_vs_vertical_proliferation_threat, empirical, 'Relative threat assessment of horizontal vs. vertical nuclear proliferation.').


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
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.73).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.76).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.82).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.84).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_arms_control_treaties).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This is one reading of the NPT Treaty of 1970. Other readings (reciprocal_disarmament_reading, withdrawal_sovereignty_reading) emphasize different articles and produce different classifications. All NPT readings are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
