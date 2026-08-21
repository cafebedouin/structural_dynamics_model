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
 *   human_readable: NPT Treaty Text (NNWS Reading): Disarmament as Binding Obligation
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story represents the Non-Nuclear-Weapon States (NNWS)
 *   reading of the Nuclear Non-Proliferation Treaty (NPT) text. In this
 *   reading, Article VI's commitment to nuclear disarmament by Nuclear-Weapon
 *   States (NWS) is a binding obligation, not merely an aspiration. NNWS
 *   non-proliferation is seen as a conditional restraint, purchasing NWS
 *   compliance with their disarmament duties. The NPT is framed as a grand
 *   bargain, where the failure of NWS to disarm creates an asymmetric burden
 *   on NNWS, making the constraint function as a Tangled Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.65).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.75).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Treaty Text (NNWS Reading): Disarmament as Binding Obligation").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'ed7a7cae-4785-4762-aa41-1da218e47780').
narrative_ontology:cs_kernel_codification('ed7a7cae-4785-4762-aa41-1da218e47780', fixed_text).
narrative_ontology:cs_authority_grounding('ed7a7cae-4785-4762-aa41-1da218e47780', lineage).
narrative_ontology:cs_interpretation_layer_present('ed7a7cae-4785-4762-aa41-1da218e47780').
narrative_ontology:cs_reading_relation('ed7a7cae-4785-4762-aa41-1da218e47780', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('ed7a7cae-4785-4762-aa41-1da218e47780', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('ed7a7cae-4785-4762-aa41-1da218e47780', foundational, disarmament_is_binding_obligation).
narrative_ontology:cs_axiom_status(disarmament_is_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ed7a7cae-4785-4762-aa41-1da218e47780', disarmament_is_binding_obligation, deontological).
narrative_ontology:cs_axiom('ed7a7cae-4785-4762-aa41-1da218e47780', foundational, non_proliferation_is_conditional_bargain).
narrative_ontology:cs_axiom_status(non_proliferation_is_conditional_bargain, holdable).
narrative_ontology:cs_axiom_grounding('ed7a7cae-4785-4762-aa41-1da218e47780', non_proliferation_is_conditional_bargain, conventional).
narrative_ontology:cs_reference_frame('ed7a7cae-4785-4762-aa41-1da218e47780', grand_bargain_reciprocity).
narrative_ontology:cs_drift_state('ed7a7cae-4785-4762-aa41-1da218e47780', post_cold_war_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ed7a7cae-4785-4762-aa41-1da218e47780', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states have renounced nuclear weapons under the NPT, expecting reciprocal disarmament from NWS. They bear the cost of restraint and vulnerability, while also benefiting from the non-proliferation norm. They actively push for NWS compliance through diplomatic channels and new treaties.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% These states are obligated by Article VI to pursue nuclear disarmament. They benefit from NNWS non-proliferation, but their own disarmament efforts are often perceived as slow or insufficient. They control the pace and interpretation of their disarmament obligations.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, beneficiary).

% The IAEA verifies NNWS compliance with non-proliferation obligations, playing a crucial role in the NPT's enforcement mechanism. It also facilitates peaceful nuclear cooperation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, analytical, global).

% Advocates for nuclear disarmament and monitors NPT compliance, particularly from NWS. Exerts moral and political pressure through campaigns and public discourse.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, global_civil_society, observer,
    moderate, generational, mobile, global).

% States that have ratified the TPNW, seeking to stigmatize and outlaw nuclear weapons entirely. They represent a challenge to the NPT's NWS-centric framework and exert pressure for more rapid disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent the proliferation of nuclear weapons globally by establishing a grand bargain: non-nuclear-weapon states forgo nuclear weapons in exchange for nuclear-weapon states committing to disarmament and access to peaceful nuclear technology.
% TRANSFER_FUNCTION: Transfers the obligation of non-proliferation from NWS to NNWS, and the obligation of disarmament from NNWS to NWS. In practice, NNWS transfer their right to develop nuclear weapons, while NWS transfer less tangible disarmament progress, leading to an asymmetric burden.
% ABSENT_VOICES: States that have not signed the NPT (e.g., India, Pakistan, Israel, North Korea) are structurally absent from the core NPT conversation, though their nuclear status and actions profoundly influence the regime's dynamics and perceived legitimacy.
% DISAPPEARANCE_RATIONALE: If the NPT and its associated norms vanished overnight, the global non-proliferation regime would collapse, likely leading to a rapid increase in nuclear weapon states, heightened regional tensions, and a significantly more unstable international security environment.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent nuclear weapons from spreading beyond the initial five nuclear powers, thereby stabilizing international security.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of large nuclear arsenals, ongoing proliferation challenges (e.g., Iran, North Korea), and numerous UN Security Council resolutions and international reports from independent bodies (e.g., SIPRI, UNIDIR) corroborate the founding problem's ongoing relevance, from sources outside the NWS.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness (0.65) reflects the ongoing cost borne by NNWS in forgoing nuclear weapons, while NWS maintain and modernize their arsenals, failing to meet their disarmament obligations. Suppression (0.75) is high due to the NWS's nuclear monopoly and the international pressure against NNWS proliferation. The theater ratio (0.5) indicates that while diplomatic efforts and Review Conferences occur regularly, concrete disarmament progress from NWS is often limited, with much activity being performative. Resistance (0.6) is substantial, as NNWS actively challenge NWS non-compliance through various diplomatic and legal avenues, including the Treaty on the Prohibition of Nuclear Weapons (TPNW).
 *
 * PERSPECTIVAL GAP:
 *   From the NNWS perspective, the NPT is a binding contract where NWS are failing to uphold their end, leading to an extractive dynamic. From the NWS perspective (as captured in a sibling reading), the NPT primarily functions as a non-proliferation regime for NNWS, with disarmament as a long-term, aspirational goal. The engine's computation of per-seat classifications will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS are both beneficiaries (from the non-proliferation norm) and victims (from the NWS's failure to disarm, leaving them vulnerable). NWS are beneficiaries (from NNWS non-proliferation) and agenda-setters (controlling the pace of disarmament). The IAEA and TPNW states act as agenda-setters and observers, pushing for compliance and a stronger disarmament norm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nws_disarmament_commitment_ambiguity,
    'Is Article VI''s disarmament commitment a binding legal obligation for NWS, or an aspirational long-term goal?',
    'International Court of Justice advisory opinion on the legal force of Article VI, or a universally adopted interpretive protocol for the NPT.',
    'If binding, the current NWS practice constitutes a breach, increasing the perceived extraction from NNWS. If aspirational, the NNWS reading''s claim of extraction is weakened, and the constraint might shift closer to a Rope for NWS.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nws_disarmament_commitment_ambiguity, conceptual, 'Ambiguity over the legal nature of NWS disarmament obligations.').

omega_variable(
    enforcement_mechanism_effectiveness,
    'How effective are NPT Review Conferences and the Treaty on the Prohibition of Nuclear Weapons (TPNW) in compelling NWS disarmament?',
    'Empirical analysis of NWS disarmament rates and policy changes directly attributable to pressure from Review Conferences or the TPNW regime.',
    'If highly effective, the NNWS''s resistance is more impactful, potentially reducing the effective extraction from their seat. If ineffective, the extraction from NNWS is amplified, as their efforts yield little reciprocal benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_effectiveness, empirical, 'Effectiveness of NNWS-led enforcement mechanisms on NWS disarmament.').

omega_variable(
    nnws_security_dilemma,
    'Do NNWS feel more secure under the NPT regime, or are they increasingly vulnerable due to NWS non-compliance with disarmament and the perceived threat of proliferation by non-signatories?',
    'Surveys of NNWS security perceptions, analysis of NNWS defense spending and alliance behaviors, and expert assessments of regional security dynamics.',
    'If NNWS feel increasingly vulnerable, the effective extraction from their seat is higher, as the security bargain is perceived as failing. If they feel secure, the coordination function is stronger, dampening extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_security_dilemma, empirical, 'NNWS security perceptions under the NPT regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nnws_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__nnws_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nnws_reading, theater_ratio, 2020, 0.5).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nnws_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__nnws_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nnws_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nnws_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__nnws_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.73).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nnws_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, global_nuclear_deterrence).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, iran_nuclear_deal).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, north_korea_nuclear_program).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'npt_treaty_text' kernel, focusing on NNWS interpretation of disarmament obligations. It is linked to sibling readings that represent NWS perspectives and withdrawal threshold debates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
