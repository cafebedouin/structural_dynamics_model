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
 *   This constraint story analyzes the Nuclear Non-Proliferation Treaty (NPT)
 *   from an 'oligopoly enforcement' reading. In this reading, the NPT
 *   primarily functions to prevent horizontal proliferation (Articles I-II)
 *   while treating vertical disarmament (Article VI) as a contingent,
 *   aspirational goal. The constraint is a Tangled Rope because it provides a
 *   genuine coordination function (preventing widespread proliferation) but
 *   does so through an asymmetric extractive mechanism that benefits the
 *   Nuclear Weapon States (P5) at the expense of Non-Nuclear Weapon States
 *   (NNWS) and threshold states. The enforcement burden falls
 *   disproportionately on NNWS, who face stringent inspections without
 *   reciprocal disarmament from the P5. This reading highlights the NPT as a
 *   mechanism for maintaining a nuclear oligopoly.
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
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'bb585c0e-2c95-48dc-af48-70d239918498').
narrative_ontology:cs_kernel_codification('bb585c0e-2c95-48dc-af48-70d239918498', fixed_text).
narrative_ontology:cs_authority_grounding('bb585c0e-2c95-48dc-af48-70d239918498', extraction).
narrative_ontology:cs_interpretation_layer_present('bb585c0e-2c95-48dc-af48-70d239918498').
narrative_ontology:cs_reading_relation('bb585c0e-2c95-48dc-af48-70d239918498', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb585c0e-2c95-48dc-af48-70d239918498', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('bb585c0e-2c95-48dc-af48-70d239918498', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('bb585c0e-2c95-48dc-af48-70d239918498', horizontal_proliferation_is_primary_threat, instrumental).
narrative_ontology:cs_axiom('bb585c0e-2c95-48dc-af48-70d239918498', foundational, p5_nuclear_arsenals_are_legitimate_deterrents).
narrative_ontology:cs_axiom_status(p5_nuclear_arsenals_are_legitimate_deterrents, holdable).
narrative_ontology:cs_axiom_grounding('bb585c0e-2c95-48dc-af48-70d239918498', p5_nuclear_arsenals_are_legitimate_deterrents, conventional).
narrative_ontology:cs_reference_frame('bb585c0e-2c95-48dc-af48-70d239918498', p5_nuclear_oligopoly_stability).
narrative_ontology:cs_drift_state('bb585c0e-2c95-48dc-af48-70d239918498', contemporary_nonproliferation_review_conferences, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('bb585c0e-2c95-48dc-af48-70d239918498', '').
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

% The five recognized nuclear weapon states (P5) who drafted the NPT. They benefit from the treaty's primary focus on preventing horizontal proliferation (Articles I-II) while maintaining their own arsenals and avoiding binding disarmament obligations (Article VI is aspirational). They enforce the inspection regime on NNWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% States that have renounced nuclear weapons under the NPT. They bear the burden of IAEA inspections and verification, limiting their sovereign nuclear programs, while receiving little to no reciprocal disarmament from the P5. Their security is theoretically enhanced by nonproliferation, but they are denied a deterrent.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states_nnws, payer,
    moderate, biographical, constrained, global).

% States with advanced nuclear capabilities that have not joined the NPT or have withdrawn. They face international sanctions and diplomatic pressure for their nuclear programs, as the NPT regime seeks to prevent their horizontal proliferation. They are denied the security benefits of a nuclear deterrent by the P5's enforcement of the oligopoly.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    powerful, biographical, identity_locked, regional).

% The international body responsible for verifying NNWS compliance with NPT safeguards. It operates under the mandate of the NPT, primarily enforcing horizontal nonproliferation, and is funded by member states, including the P5. Its mandate is to ensure nuclear technology is used for peaceful purposes, but its enforcement is asymmetric.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, international_atomic_energy_agency_iaea, agenda_setter,
    organized, generational, constrained, global).

% NGOs and international organizations that advocate for universal nuclear disarmament and nonproliferation. They observe the NPT's operation, often highlighting the asymmetry between P5 and NNWS obligations, and push for stronger enforcement of Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, global_nonproliferation_advocates, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the horizontal proliferation of nuclear weapons by establishing a global norm against new nuclear weapon states and providing a framework for international verification and peaceful nuclear technology transfer.
% TRANSFER_FUNCTION: Transfers the burden of verification and renunciation of nuclear weapons to non-nuclear weapon states, in exchange for theoretical security guarantees and access to peaceful nuclear technology, while preserving the nuclear monopoly of the P5.
% ABSENT_VOICES: States that have developed nuclear weapons outside the NPT (e.g., India, Pakistan, Israel, North Korea) are structurally excluded from the regime's benefits and are targets of its enforcement. They would argue for a security environment that justifies their deterrent and challenge the legitimacy of the P5's monopoly.
% DISAPPEARANCE_RATIONALE: If the NPT vanished overnight, the global nuclear order would rapidly destabilize. Many NNWS would likely pursue nuclear weapons for security, leading to a cascade of proliferation, increased regional conflicts, and a breakdown of international arms control efforts. The P5's nuclear oligopoly would be challenged by new entrants.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the fear of widespread nuclear proliferation following the Cuban Missile Crisis and the development of nuclear weapons by additional states (e.g., China, France).
% FOUNDING_PROBLEM_CORROBORATION: The P5 and IAEA consistently attest that the threat of proliferation remains live, citing ongoing challenges from threshold states and the risk of nuclear terrorism. Global nonproliferation advocates corroborate the continued threat but emphasize the P5's failure to disarm as a primary driver of proliferation risk.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.78) because NNWS forgo a key security option (nuclear deterrence) and bear significant inspection costs, while the P5 retain their arsenals. Suppression is very high (0.85) due to the severe international penalties (sanctions, isolation, potential military action) faced by states that pursue nuclear weapons outside the NPT framework. The theater ratio is moderate (0.45) because while the nonproliferation function is real, the P5's commitment to disarmament (Article VI) is often performative, with little concrete action, serving to legitimize the unequal regime. The metrics show a trend of increasing extractiveness and suppression over time as the regime has hardened.
 *
 * PERSPECTIVAL GAP:
 *   The P5 perceive the NPT as a successful, essential coordination mechanism for global security. NNWS and threshold states, particularly those facing regional threats, experience it as an extractive constraint that denies them sovereign security options while perpetuating an unequal nuclear order. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 are clear beneficiaries and agenda-setters, as the NPT preserves their nuclear monopoly and global power status (low directionality). NNWS are payers, bearing the costs of nonproliferation without the benefits of disarmament (high directionality). Threshold states are also payers, facing direct suppression for challenging the oligopoly. The IAEA, while a key enforcer, is constrained by the P5's agenda, making it an institutional actor with moderate directionality, primarily enforcing the horizontal nonproliferation aspect.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status,
    'Is Article VI (disarmament obligation) a binding legal obligation with temporal urgency, or a contingent, aspirational goal?',
    'International Court of Justice advisory opinion on the legal status and enforceability of Article VI, or a new treaty explicitly setting a timeline for P5 disarmament.',
    'If binding and urgent, the NPT''s extractiveness would decrease for NNWS, and the P5''s directionality would shift towards being targets of disarmament, potentially reclassifying the constraint towards a Rope or even a Scaffold (if a clear disarmament timeline were set). If aspirational, the current Tangled Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_status, conceptual, 'Ambiguity regarding the legal force and urgency of the P5''s disarmament obligations under Article VI.').

omega_variable(
    security_dilemma_justification,
    'To what extent does the security dilemma (states seeking deterrents due to perceived threats) justify the pursuit of nuclear weapons by threshold states, and how does this interact with the NPT''s nonproliferation goals?',
    'Independent security assessments for threshold states, coupled with P5 security guarantees or regional security frameworks that address their perceived threats without requiring nuclear weapons.',
    'If the security dilemma is a strong, unaddressed driver, the NPT''s suppression of threshold states is more clearly extractive and less justifiable as pure coordination. This would increase the effective extractiveness for threshold states and strengthen the Snare-like aspects of the constraint for them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_dilemma_justification, empirical, 'The role of the security dilemma in motivating nuclear proliferation and its impact on the NPT''s legitimacy.').

omega_variable(
    oligopoly_stability_vs_proliferation_risk,
    'Does the P5''s nuclear oligopoly inherently increase or decrease the long-term risk of proliferation?',
    'Longitudinal studies comparing proliferation rates in periods of P5 disarmament efforts versus periods of P5 modernization, or counterfactual modeling of a world without the NPT''s asymmetric structure.',
    'If the oligopoly is found to increase proliferation risk (by incentivizing NNWS to seek parity), the NPT''s coordination function is undermined, and its classification shifts more towards a Snare. If it demonstrably decreases risk, the coordination function is strengthened, potentially moving it closer to a Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(oligopoly_stability_vs_proliferation_risk, empirical, 'Whether the NPT''s asymmetric structure contributes to or mitigates global proliferation risks.').


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
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, ctbt_treaty_1996).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iran_nuclear_deal_jcpoa).

% DUAL FORMULATION NOTE:
% This is one of three readings of the NPT Treaty (npt_treaty_1970). This 'oligopoly enforcement' reading emphasizes horizontal nonproliferation (Articles I-II) and views Article VI (disarmament) as aspirational. It is linked to the 'reciprocal disarmament' reading and the 'withdrawal sovereignty' reading, which offer alternative interpretations of the treaty's core obligations and rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
