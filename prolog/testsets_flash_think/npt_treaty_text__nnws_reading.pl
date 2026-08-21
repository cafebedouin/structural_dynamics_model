% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT: Non-Nuclear Weapon States' Disarmament Obligation Reading
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story represents the Non-Nuclear Weapon States' (NNWS)
 *   reading of the Nuclear Non-Proliferation Treaty (NPT), emphasizing
 *   Article VI as a binding obligation for Nuclear Weapon States (NWS) to
 *   pursue nuclear disarmament. The NNWS's commitment to non-proliferation is
 *   seen as conditional on NWS compliance with this disarmament obligation.
 *   The constraint is claimed as a 'rope' from the NNWS perspective, as it
 *   serves as a coordination mechanism for their collective security and
 *   diplomatic pressure. However, the authored metrics reflect the
 *   substantial burden of non-proliferation on NNWS and the NWS's resistance
 *   to immediate disarmament, leading to a moderate-to-high extractiveness
 *   and suppression for NNWS.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.6).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.7).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT: Non-Nuclear Weapon States' Disarmament Obligation Reading").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'cf655ced-502f-4e36-866a-6c4eecfa7127').
narrative_ontology:cs_kernel_codification('cf655ced-502f-4e36-866a-6c4eecfa7127', fixed_text).
narrative_ontology:cs_authority_grounding('cf655ced-502f-4e36-866a-6c4eecfa7127', lineage).
narrative_ontology:cs_interpretation_layer_present('cf655ced-502f-4e36-866a-6c4eecfa7127').
narrative_ontology:cs_reading_relation('cf655ced-502f-4e36-866a-6c4eecfa7127', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf655ced-502f-4e36-866a-6c4eecfa7127', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('cf655ced-502f-4e36-866a-6c4eecfa7127', foundational, disarmament_is_binding_obligation).
narrative_ontology:cs_axiom_status(disarmament_is_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cf655ced-502f-4e36-866a-6c4eecfa7127', disarmament_is_binding_obligation, deontological).
narrative_ontology:cs_axiom('cf655ced-502f-4e36-866a-6c4eecfa7127', foundational, non_proliferation_is_conditional).
narrative_ontology:cs_axiom_status(non_proliferation_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('cf655ced-502f-4e36-866a-6c4eecfa7127', non_proliferation_is_conditional, conventional).
narrative_ontology:cs_reference_frame('cf655ced-502f-4e36-866a-6c4eecfa7127', original_npt_bargain).
narrative_ontology:cs_drift_state('cf655ced-502f-4e36-866a-6c4eecfa7127', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf655ced-502f-4e36-866a-6c4eecfa7127', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_security).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states commit to not acquiring nuclear weapons, bearing the cost of foregoing a perceived security option. In return, they gain collective security and a platform to demand disarmament from NWS. Their non-proliferation is conditional on NWS fulfilling Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% These states are obligated under Article VI to pursue nuclear disarmament. From the NNWS reading, they are the target of this obligation, but they also benefit from the NNWS's non-proliferation. They largely control the pace and interpretation of disarmament efforts.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, beneficiary).

% The IAEA is responsible for verifying the non-proliferation commitments of NNWS through safeguards. It provides technical expertise and reports on compliance, acting as a key enforcement mechanism for the non-proliferation aspect.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, biographical, analytical, global).

% These groups actively campaign for nuclear disarmament, supporting the NNWS interpretation of Article VI. They are excluded from direct treaty negotiations but exert pressure through public advocacy and lobbying.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, civil_society_disarmament_advocates, excluded,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, civil_society_disarmament_advocates, observer).

% States that have joined the TPNW see it as a means to advance the disarmament agenda, often viewing it as a way to fulfill the spirit of NPT Article VI where the NPT itself has failed. They bear diplomatic costs for challenging the NPT status quo.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective efforts of Non-Nuclear Weapon States to pressure Nuclear Weapon States towards disarmament, while maintaining a global norm against nuclear proliferation.
% TRANSFER_FUNCTION: Transfers the burden of non-proliferation from Nuclear Weapon States to Non-Nuclear Weapon States, in exchange for a binding, albeit often unfulfilled, promise of future disarmament from NWS.
% ABSENT_VOICES: States that have withdrawn from the NPT (e.g., North Korea) or never joined (e.g., India, Pakistan, Israel) are absent. They would argue that the treaty is inherently discriminatory and has failed to deliver on its disarmament promise, justifying their own nuclear programs or non-participation.
% DISAPPEARANCE_RATIONALE: The NPT is a foundational pillar of global security. Its disappearance would likely lead to widespread nuclear proliferation, a collapse of arms control regimes, and a fundamental reorganization of international power dynamics and security alliances.
% FOUNDING_PROBLEM: To prevent the spread of nuclear weapons technology to more states, while simultaneously committing existing nuclear powers to pursue disarmament, thereby creating a pathway to a world free of nuclear weapons.
% FOUNDING_PROBLEM_CORROBORATION: Non-Nuclear Weapon States and civil society groups attest that the disarmament problem, particularly the NWS's compliance with Article VI, is still live and largely unaddressed. Nuclear Weapon States claim disarmament is an ongoing, complex process. Independent international law scholars and historians corroborate the dual intent of the treaty, noting the persistent tension between non-proliferation and disarmament.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) reflects the ongoing cost to NNWS of foregoing nuclear weapons, a cost that feels increasingly uncompensated due to perceived NWS inaction on disarmament. Suppression (0.70) is high because NNWS are legally bound by the treaty and face significant international pressure and sanctions if they violate non-proliferation norms. Theater ratio (0.20) is low, indicating that NNWS diplomatic efforts at Review Conferences and through initiatives like the TPNW are genuine attempts to enforce Article VI, not mere performance. Resistance (0.70) is high, primarily from NWS who resist interpretations of Article VI as requiring immediate, time-bound disarmament. Accessibility collapse (0.50) is moderate; while withdrawal from the NPT is possible, it carries severe diplomatic and economic costs, and alternative security arrangements are limited.
 *
 * PERSPECTIVAL GAP:
 *   The NNWS reading views the NPT as a grand bargain where their non-proliferation is conditional on NWS disarmament. From this perspective, the NWS are failing to uphold their end, making the constraint increasingly extractive for NNWS. The NWS reading (a sibling constraint, not authored here) would emphasize the success of non-proliferation and view disarmament as an aspirational, long-term goal, experiencing the constraint as a successful coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-Nuclear Weapon States are both payers (bearing the cost of non-proliferation) and beneficiaries (gaining collective security and a platform for disarmament advocacy). Nuclear Weapon States are agenda-setters (controlling the pace of disarmament) and beneficiaries (maintaining their arsenals while NNWS do not proliferate). The IAEA acts as an agenda-setter for verification. Civil society and TPNW states are excluded from direct treaty power but act as observers and exert pressure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness_ambiguity,
    'Is Article VI of the NPT a legally binding, immediately actionable obligation for NWS to disarm, or an aspirational commitment to be pursued in good faith?',
    'A definitive ruling by the International Court of Justice on the precise legal nature and timeline of Article VI obligations, or a new, universally ratified treaty specifying disarmament steps and timelines.',
    'If binding and immediate, the NPT''s extractiveness on NNWS would be seen as more legitimate, and NWS would face increased pressure. If aspirational, the NNWS''s non-proliferation burden would be seen as less conditional, increasing its effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_bindingness_ambiguity, conceptual, 'Ambiguity over the legal force of NPT Article VI''s disarmament clause.').

omega_variable(
    nws_disarmament_sincerity,
    'Are NWS genuinely committed to nuclear disarmament, or is their stated commitment primarily a diplomatic performance to maintain the non-proliferation regime?',
    'Observable, verifiable reductions in nuclear arsenals, cessation of modernization programs, and concrete steps towards a disarmament treaty, independent of geopolitical considerations.',
    'If NWS are found to be performing, the theater_ratio of the NPT would increase significantly, and the NNWS''s extractiveness would be amplified due to perceived bad faith. If genuine, the NNWS''s burden would be seen as a necessary, temporary cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_disarmament_sincerity, empirical, 'Sincerity of NWS commitment to disarmament.').

omega_variable(
    nnws_leverage_effectiveness,
    'How effective is the collective diplomatic pressure from NNWS (e.g., NPT Review Conferences, TPNW) in compelling NWS to fulfill Article VI?',
    'Empirical analysis of NWS policy changes directly attributable to NNWS pressure, or a shift in NWS rhetoric and action following significant NNWS initiatives.',
    'If NNWS leverage is low, their ''rope'' classification becomes more tenuous, potentially shifting towards a ''snare'' as their non-proliferation is uncompensated. If high, the ''rope'' classification is strengthened, as their coordination yields tangible results.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nnws_leverage_effectiveness, empirical, 'Effectiveness of NNWS diplomatic leverage on NWS disarmament.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nnws_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__nnws_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nnws_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__nnws_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nnws_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__nnws_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nnws_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. It focuses on the NNWS interpretation of Article VI disarmament obligations. Sibling constraints represent alternative readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
