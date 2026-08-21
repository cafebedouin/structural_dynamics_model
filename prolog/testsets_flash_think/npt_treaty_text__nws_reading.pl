% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT: Non-Proliferation as Binding (NWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint represents the Nuclear-Weapon States' (NWS) reading of
 *   the Nuclear Non-Proliferation Treaty (NPT) text. In this interpretation,
 *   non-proliferation for Non-Nuclear-Weapon States (NNWS) is a binding and
 *   strictly enforced obligation, while NWS disarmament under Article VI is
 *   treated as an aspirational, long-term goal without concrete enforcement
 *   mechanisms or timelines. This reading benefits NWS by maintaining their
 *   nuclear monopoly and strategic advantage, while imposing significant
 *   constraints and verification burdens on NNWS. The high extractiveness and
 *   suppression reflect the asymmetry of this arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.85).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.9).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT: Non-Proliferation as Binding (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '8a618f12-91a8-41f4-8c43-2a255f08885d').
narrative_ontology:cs_kernel_codification('8a618f12-91a8-41f4-8c43-2a255f08885d', fixed_text).
narrative_ontology:cs_authority_grounding('8a618f12-91a8-41f4-8c43-2a255f08885d', extraction).
narrative_ontology:cs_interpretation_layer_present('8a618f12-91a8-41f4-8c43-2a255f08885d').
narrative_ontology:cs_reading_relation('8a618f12-91a8-41f4-8c43-2a255f08885d', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a618f12-91a8-41f4-8c43-2a255f08885d', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('8a618f12-91a8-41f4-8c43-2a255f08885d', foundational, non_proliferation_is_binding_obligation).
narrative_ontology:cs_axiom_status(non_proliferation_is_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('8a618f12-91a8-41f4-8c43-2a255f08885d', non_proliferation_is_binding_obligation, conventional).
narrative_ontology:cs_axiom('8a618f12-91a8-41f4-8c43-2a255f08885d', foundational, disarmament_is_aspirational_long_term_goal).
narrative_ontology:cs_axiom_status(disarmament_is_aspirational_long_term_goal, holdable).
narrative_ontology:cs_axiom_grounding('8a618f12-91a8-41f4-8c43-2a255f08885d', disarmament_is_aspirational_long_term_goal, conventional).
narrative_ontology:cs_reference_frame('8a618f12-91a8-41f4-8c43-2a255f08885d', nws_nuclear_monopoly_stability).
narrative_ontology:cs_drift_state('8a618f12-91a8-41f4-8c43-2a255f08885d', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8a618f12-91a8-41f4-8c43-2a255f08885d', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_deterrence_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the recognized nuclear powers, they interpret the NPT to prioritize non-proliferation by NNWS while treating their own disarmament obligations as aspirational and long-term. They benefit from maintaining their nuclear monopoly and global strategic advantage, enforcing non-proliferation through diplomatic, economic, and military means.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary).

% These states are bound by strict non-proliferation commitments, including IAEA safeguards, and face severe international consequences for non-compliance. They bear the cost of foregoing nuclear weapons development, often feeling that NWS have not upheld their reciprocal disarmament obligations under Article VI. Their collective power is limited by NWS's strategic dominance.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, non_nuclear_weapon_states, excluded).

% The IAEA is mandated to verify NNWS compliance with non-proliferation obligations through safeguards. Its budget and mandate are heavily focused on horizontal proliferation (NNWS acquiring weapons), reflecting the NWS reading's priorities. It operates within the political constraints set by its member states, including the NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, biographical, constrained, global).

% Advocates for universal nuclear disarmament, they are largely excluded from the core decision-making processes of the NPT regime. They bear the diffuse cost of continued nuclear risk and the moral burden of the NWS's unfulfilled disarmament promises. Their commitment to disarmament is often deeply ideological.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, global_civil_society_disarmament_advocates, excluded,
    powerless, generational, identity_locked, global).

% The United States, United Kingdom, and Russian Federation serve as depositaries of the NPT. In this role, they hold significant interpretive power over the treaty's implementation and evolution, reinforcing the NWS reading by controlling formal processes and diplomatic narratives.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, treaty_depositary_states, agenda_setter,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT coordinates international efforts to prevent the spread of nuclear weapons to additional states, aiming to reduce the risk of nuclear war and maintain global strategic stability.
% TRANSFER_FUNCTION: This reading of the NPT transfers the right to possess nuclear weapons from non-nuclear-weapon states to nuclear-weapon states, in exchange for an unenforced commitment by NWS to eventual disarmament. NNWS transfer sovereignty over their nuclear programs and accept intrusive verification.
% ABSENT_VOICES: Non-signatory states (e.g., India, Pakistan, Israel, North Korea) are structurally absent from the NPT's internal discourse, as they reject its premise of a two-tiered nuclear order. Global civil society disarmament advocates are also largely excluded, consistently calling for immediate and verifiable disarmament from NWS.
% DISAPPEARANCE_RATIONALE: If the NPT and its associated enforcement mechanisms vanished overnight, the global non-proliferation regime would collapse. Many NNWS would likely pursue nuclear weapons, leading to a rapid and dangerous proliferation cascade, fundamentally altering global security dynamics and increasing the risk of nuclear conflict.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent further proliferation after the initial nuclear arms race, particularly to avoid a world with dozens of nuclear-armed states.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear-Weapon States governments and their strategic analysts attest that the proliferation problem is still live and the NPT is essential for global stability. Many Non-Nuclear-Weapon States governments, UN bodies, and independent arms control experts attest that the founding problem of NWS disarmament remains largely unaddressed, and the NWS commitment is unfulfilled, leading to a contested status of the treaty's original bargain.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because NNWS bear the full burden of non-proliferation without a clear reciprocal commitment from NWS, effectively transferring a strategic advantage. Suppression is very high (0.90) due to the robust international safeguards regime, diplomatic pressure, and potential sanctions or military action against NNWS that might violate the treaty. The theater ratio is moderate-high (0.60) as NWS engage in performative disarmament talks and incremental reductions that do not fundamentally challenge their nuclear arsenals, while the core function of the treaty (non-proliferation) is actively enforced. Accessibility collapse is high for NNWS, as the international system severely restricts alternatives to NPT compliance. Resistance is moderate-high from NNWS who feel the asymmetry and advocate for more robust NWS disarmament.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS perspective, this arrangement is a necessary coordination mechanism for global stability, with their disarmament being a complex, long-term process. From the NNWS perspective, it is an extractive structure that perpetuates a nuclear apartheid, where their security is constrained while NWS maintain their arsenals. The engine's per-seat classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-Weapon States are the primary beneficiaries and agenda-setters (low directionality), as they define the terms of compliance and maintain their strategic advantage. Non-Nuclear-Weapon States are the primary targets/payers (high directionality), bearing the costs of non-proliferation without equivalent benefits. The IAEA, while an enforcer, is constrained by the NWS's interpretation. Global civil society advocates are excluded and bear diffuse costs, pushing their directionality towards the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The NWS reading of the NPT prevents mislabeling the non-proliferation function as pure extraction by acknowledging the coordination problem it solves (preventing nuclear spread). However, it risks mislabeling the NWS's disarmament obligations as genuine coordination rather than a cover story for maintaining a nuclear monopoly. The high theater ratio and contested founding problem status highlight the mandatrophy of the disarmament aspect, where the original mandate for NWS disarmament has atrophied into performance, while the non-proliferation mandate remains robustly enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disarmament_timeline_ambiguity,
    'Is the NWS commitment to disarmament ''at an early date'' (Article VI) a genuine, albeit unfulfilled, obligation, or was it always an aspirational placeholder to secure NNWS buy-in?',
    'Analysis of NWS internal policy documents from the NPT''s drafting period, and a comparison of NWS disarmament rates with their stated commitments over time.',
    'If it was always a placeholder, the extractiveness from NNWS is higher, as the reciprocal promise was never truly intended. If it was a genuine obligation, the NWS''s current position represents a significant drift from the treaty''s original intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_timeline_ambiguity, conceptual, 'Ambiguity regarding the sincerity and enforceability of NWS disarmament commitments.').

omega_variable(
    iaea_budget_allocation_bias,
    'Does the IAEA''s budget and operational focus disproportionately prioritize horizontal proliferation (NNWS) verification over vertical proliferation (NWS disarmament verification and transparency)?',
    'Detailed analysis of IAEA budget allocations, staffing, and inspection mandates across NWS and NNWS programs, compared against the stated goals of the NPT.',
    'If biased, it would confirm the NWS reading''s structural influence on the enforcement body, further highlighting the asymmetry and extractiveness of the regime from the NNWS perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iaea_budget_allocation_bias, empirical, 'Whether IAEA resources reflect an equitable approach to proliferation verification.').

omega_variable(
    nuclear_deterrence_legitimacy,
    'Is nuclear deterrence a stable and legitimate basis for global security, or does it inherently carry an unacceptable risk of catastrophic war, making NWS arsenals a global liability rather than a benefit?',
    'Ongoing geopolitical events, academic studies on deterrence stability, and ethical/philosophical debates on the morality of nuclear weapons. No definitive empirical resolution is expected.',
    'If deterrence is deemed illegitimate, the NWS''s claim of providing ''stability'' becomes a cover story, increasing the perceived extractiveness and moral cost of their nuclear monopoly for all states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_deterrence_legitimacy, preference, 'The fundamental legitimacy of nuclear deterrence as a security paradigm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__nws_reading, theater_ratio, 1990, 0.45).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.52).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.57).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nws_reading, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__nws_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nws_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__nws_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nws_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nuclear_weapons_ban_treaty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. It focuses on the NWS interpretation, where non-proliferation is binding for NNWS and disarmament is aspirational for NWS. Sibling readings (nnws_reading, withdrawal_threshold_reading) offer alternative interpretations of the same kernel, with different ε values and stakeholder positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
