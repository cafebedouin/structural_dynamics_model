% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV/VI Grand Bargain Interpretation
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'grand bargain' interpretation of the
 *   Nuclear Non-Proliferation Treaty (NPT), where non-nuclear-weapon states
 *   (NNWS) commit to non-proliferation (Article IV) in exchange for
 *   nuclear-weapon states (NWS) committing to disarmament (Article VI). This
 *   reading emphasizes the reciprocal and conditional nature of these
 *   obligations, asserting that NWS failure to disarm undermines the
 *   legitimacy of NNWS non-proliferation. It is a contested reading of the
 *   NPT kernel, distinct from 'nonproliferation_primary' (which prioritizes
 *   Article IV) and 'abolitionist' (which prioritizes Article VI as an
 *   absolute mandate).
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states_grand_bargain: Agenda setter (institutional/constrained) — benefits from non-proliferation, resists disarmament.
 *   - non_nuclear_weapon_states_grand_bargain: Beneficiary (organized/constrained) — adheres to Article IV, expects Article VI fulfillment.
 *   - non_nuclear_weapon_states_disarmament_advocates: Payer (moderate/identity_locked) — actively pushes for disarmament, bears diplomatic costs.
 *   - global_security_regime: Beneficiary (institutional/constrained) — benefits from non-proliferation, but legitimacy eroded by NWS inaction.
 *   - international_atomic_energy_agency: Observer (institutional/analytical) — verifies safeguards, reports on proliferation risks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.65).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.7).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV/VI Grand Bargain Interpretation").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'ba0befbd-3766-44b7-b18c-be2f99096620').
narrative_ontology:cs_kernel_codification('ba0befbd-3766-44b7-b18c-be2f99096620', fixed_text).
narrative_ontology:cs_authority_grounding('ba0befbd-3766-44b7-b18c-be2f99096620', lineage).
narrative_ontology:cs_interpretation_layer_present('ba0befbd-3766-44b7-b18c-be2f99096620').
narrative_ontology:cs_reading_relation('ba0befbd-3766-44b7-b18c-be2f99096620', npt_article_iv_vi_pairing__nonproliferation_primary, influences).
narrative_ontology:cs_reading_relation('ba0befbd-3766-44b7-b18c-be2f99096620', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('ba0befbd-3766-44b7-b18c-be2f99096620', foundational, nonproliferation_conditional_on_disarmament).
narrative_ontology:cs_axiom_status(nonproliferation_conditional_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('ba0befbd-3766-44b7-b18c-be2f99096620', nonproliferation_conditional_on_disarmament, conventional).
narrative_ontology:cs_axiom('ba0befbd-3766-44b7-b18c-be2f99096620', foundational, disarmament_obligation_enforceable).
narrative_ontology:cs_axiom_status(disarmament_obligation_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('ba0befbd-3766-44b7-b18c-be2f99096620', disarmament_obligation_enforceable, conventional).
narrative_ontology:cs_reference_frame('ba0befbd-3766-44b7-b18c-be2f99096620', original_npt_negotiations_intent).
narrative_ontology:cs_drift_state('ba0befbd-3766-44b7-b18c-be2f99096620', contemporary_npt_review_conferences, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ba0befbd-3766-44b7-b18c-be2f99096620', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_grand_bargain).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, global_security_regime).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_grand_bargain).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_disarmament_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Obligated to pursue disarmament under Article VI, but often interpret this as a long-term aspiration rather than an immediate, enforceable commitment. They benefit from the non-proliferation aspect (Article IV) while resisting concrete steps towards their own disarmament, creating an asymmetry. Their exit is constrained by the political and security implications of abandoning the NPT.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states_grand_bargain, agenda_setter,
    institutional, generational, constrained, global).

% Adhere to Article IV, foregoing nuclear weapons, on the understanding that weapon states will disarm. They benefit from the perceived security of a non-proliferation regime but bear the cost of weapon states' slow disarmament. Their exit options include withdrawal from the NPT, but this carries significant diplomatic and security costs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_grand_bargain, beneficiary,
    organized, generational, constrained, global).

% Actively push for weapon states to fulfill their Article VI obligations, viewing the current pace as a breach of the 'grand bargain.' They bear the cost of maintaining the moral and legal pressure for disarmament, often facing diplomatic resistance. Their commitment is often identity-locked to the principle of nuclear abolition.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states_disarmament_advocates, payer,
    moderate, generational, identity_locked, global).

% Benefits from the NPT's role in preventing horizontal proliferation, which is seen as a cornerstone of international stability. However, the regime's legitimacy is eroded by the perceived failure of weapon states to disarm, creating a long-term risk to the entire structure. Its exit is constrained by the catastrophic consequences of NPT collapse.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, global_security_regime, beneficiary,
    institutional, civilizational, constrained, universal).

% Verifies compliance with Article III (safeguards) but has no direct mandate to enforce Article VI disarmament. It observes the political dynamics and reports on proliferation risks, providing technical expertise that underpins the regime's verification aspects.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global nuclear non-proliferation by establishing a framework where non-nuclear-weapon states (NNWS) forgo nuclear weapons in exchange for peaceful nuclear technology and a commitment from nuclear-weapon states (NWS) to disarm.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons from NNWS to NWS, in exchange for a promise of disarmament and access to peaceful nuclear technology. The transfer of disarmament progress from NWS to NNWS is largely unfulfilled.
% ABSENT_VOICES: States that have never joined the NPT (e.g., India, Pakistan, Israel, North Korea) are absent. They would argue that the NPT is inherently discriminatory and that their security concerns justify their nuclear status, challenging the NPT's universal applicability.
% DISAPPEARANCE_RATIONALE: If the grand bargain interpretation vanished, the NPT's legitimacy would collapse, potentially leading to widespread nuclear proliferation as NNWS would no longer feel bound by Article IV without the reciprocal disarmament obligation. The global security architecture would be fundamentally destabilized.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the spread of nuclear weapons to more states, while acknowledging the existing nuclear powers.
% FOUNDING_PROBLEM_CORROBORATION: The threat of nuclear war and proliferation remains live, attested by UN resolutions, international security analyses, and the ongoing efforts of non-proliferation advocates. The NPT's continued existence, despite its flaws, is seen as essential by a broad consensus of states and international organizations, corroborating the problem's persistence.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates non-proliferation efforts (benefiting NNWS and global security) but simultaneously involves asymmetric extraction: NWS benefit from NNWS restraint while largely failing to meet their own disarmament obligations. This asymmetry requires active enforcement (diplomatic pressure, review conferences) to maintain. Extractiveness (0.65) is high due to the unfulfilled promise of disarmament. Suppression (0.70) reflects the diplomatic and security costs for NNWS to withdraw or challenge the NPT. Theater ratio (0.40) indicates that a significant portion of NWS disarmament rhetoric and activity is performative, masking a lack of genuine progress.
 *
 * PERSPECTIVAL GAP:
 *   NWS experience this as a Rope, where they coordinate non-proliferation with minimal cost to their own arsenals. NNWS, particularly disarmament advocates, experience it as a Snare or highly extractive Tangled Rope, where their restraint is extracted without the promised reciprocal disarmament. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are beneficiaries (d near 0.0) as they maintain their arsenals while NNWS forgo theirs. NNWS are payers (d near 1.0) as they bear the cost of non-proliferation without full reciprocity. Disarmament advocates are targets (d near 1.0) due to their identity-locked commitment and the costs of their advocacy. The global security regime is a beneficiary (d near 0.0) of the non-proliferation aspect, but its long-term stability is undermined by the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'grand bargain' interpretation prevents mislabeling the NPT as a pure Rope (which would ignore the disarmament deficit) or a pure Snare (which would ignore the genuine non-proliferation coordination). It highlights the drift from its original mandate where disarmament was a more central, enforceable obligation, towards a state where non-proliferation is enforced while disarmament is largely aspirational. The contested status of the 'founding problem' (whether the disarmament problem is still 'live' in the same way) is central to this mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_identity,
    'Is this constraint a genuine ''grand bargain'' reading of the NPT, or is it a ''nonproliferation primary'' reading with a rhetorical nod to disarmament?',
    'Analysis of NWS actions: if NWS consistently prioritize non-proliferation over disarmament, and resist any linkage, it leans towards ''nonproliferation primary''. If NWS actively pursue disarmament and acknowledge its linkage to non-proliferation, it supports ''grand bargain''.',
    'If ''nonproliferation primary'', the constraint''s extractiveness from NNWS is higher, and the NWS''s role shifts from agenda-setter with a disarmament obligation to a pure beneficiary of non-proliferation. If ''abolitionist'', the constraint''s legitimacy is fundamentally challenged by the mere existence of nuclear weapons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_kernel_reading_identity, conceptual, 'Ambiguity in the NPT''s core interpretation regarding the balance of non-proliferation and disarmament obligations.').

omega_variable(
    disarmament_verifiability,
    'Are current verification technologies and political will sufficient to credibly verify nuclear disarmament, or does the technical challenge itself suppress disarmament progress?',
    'Technical assessments by international bodies (e.g., IAEA, CTBTO) on the feasibility and robustness of disarmament verification regimes, coupled with political commitments to implement them.',
    'If verification is technically infeasible or politically suppressed, the NWS''s failure to disarm is partly structural, reducing the perceived extractiveness of the ''grand bargain'' from NNWS. If feasible but unpursued, it increases the perceived extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_verifiability, empirical, 'The role of verification challenges in the slow pace of nuclear disarmament.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(npt__tr_t1990, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(npt__be_t1990, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(npt__su_t1990, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, treaty_on_the_prohibition_of_nuclear_weapons).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Article IV/VI pairing kernel. It emphasizes the reciprocal nature of non-proliferation and disarmament obligations. Sibling readings include 'nonproliferation_primary' (prioritizing non-proliferation) and 'abolitionist' (prioritizing complete disarmament).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
