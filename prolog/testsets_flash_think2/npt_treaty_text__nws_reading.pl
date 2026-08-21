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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Treaty: NWS Reading (Non-Proliferation Binding, Disarmament Aspirational)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint represents the Nuclear Weapon States' (NWS) reading of
 *   the Nuclear Non-Proliferation Treaty (NPT), where non-proliferation is a
 *   binding obligation for Non-Nuclear Weapon States (NNWS), while
 *   disarmament for NWS is an aspirational, long-term goal without immediate
 *   enforcement. This interpretation prioritizes horizontal non-proliferation
 *   and maintains the existing nuclear order. The high extractiveness and
 *   suppression reflect the burden placed on NNWS and the robust enforcement
 *   mechanisms (IAEA safeguards, UNSC sanctions) primarily targeting NNWS.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.85).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.9).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Treaty: NWS Reading (Non-Proliferation Binding, Disarmament Aspirational)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '6bd4d3c9-b8c9-4887-ad31-919e90de4323').
narrative_ontology:cs_kernel_codification('6bd4d3c9-b8c9-4887-ad31-919e90de4323', fixed_text).
narrative_ontology:cs_authority_grounding('6bd4d3c9-b8c9-4887-ad31-919e90de4323', extraction).
narrative_ontology:cs_interpretation_layer_present('6bd4d3c9-b8c9-4887-ad31-919e90de4323').
narrative_ontology:cs_reading_relation('6bd4d3c9-b8c9-4887-ad31-919e90de4323', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_reading_relation('6bd4d3c9-b8c9-4887-ad31-919e90de4323', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('6bd4d3c9-b8c9-4887-ad31-919e90de4323', foundational, non_proliferation_is_binding_norm).
narrative_ontology:cs_axiom_status(non_proliferation_is_binding_norm, holdable).
narrative_ontology:cs_axiom_grounding('6bd4d3c9-b8c9-4887-ad31-919e90de4323', non_proliferation_is_binding_norm, deontological).
narrative_ontology:cs_axiom('6bd4d3c9-b8c9-4887-ad31-919e90de4323', foundational, disarmament_is_aspirational_goal).
narrative_ontology:cs_axiom_status(disarmament_is_aspirational_goal, holdable).
narrative_ontology:cs_axiom_grounding('6bd4d3c9-b8c9-4887-ad31-919e90de4323', disarmament_is_aspirational_goal, conventional).
narrative_ontology:cs_reference_frame('6bd4d3c9-b8c9-4887-ad31-919e90de4323', npt_original_intent_nws_view).
narrative_ontology:cs_drift_state('6bd4d3c9-b8c9-4887-ad31-919e90de4323', contemporary_geopolitical_landscape, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6bd4d3c9-b8c9-4887-ad31-919e90de4323', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_deterrence_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear weapons and interpret the NPT as primarily binding non-proliferation obligations on NNWS, while their own disarmament is an aspirational, long-term goal. They benefit from the security stability of a limited nuclear club and control the enforcement mechanisms.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Are bound by non-proliferation obligations, including intrusive IAEA safeguards, and forgo nuclear weapons development. They bear the costs of verification and the perceived security asymmetry, often viewing NWS disarmament commitments as unfulfilled.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% Administers and enforces safeguards on NNWS to verify non-diversion of nuclear material. Its budget and mandate are heavily focused on horizontal proliferation, reflecting the NWS reading's priorities.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea, agenda_setter,
    institutional, biographical, constrained, global).

% Holds the power to impose sanctions and other enforcement measures against states found in non-compliance with non-proliferation obligations. Its permanent members include NWS, giving them significant control over enforcement actions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, un_security_council, agenda_setter,
    institutional, biographical, constrained, global).

% Advocates for universal nuclear disarmament and criticizes the perceived asymmetry of the NPT. While influential in public discourse, it lacks direct enforcement power within the treaty framework.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, global_civil_society, excluded,
    moderate, generational, mobile, global).

% Academics, think tanks, and policy analysts who study the NPT's effectiveness, its challenges, and the differing interpretations of its articles. They provide independent analysis but do not directly participate in enforcement or policy-making.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, analytical_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global norm against the spread of nuclear weapons, providing a framework for international cooperation on peaceful nuclear energy while preventing horizontal proliferation and maintaining a stable international order with a limited number of nuclear powers.
% TRANSFER_FUNCTION: Transfers security benefits (reduced proliferation risk, nuclear deterrence stability) to Nuclear Weapon States (NWS) and their allies, while transferring the burden of non-acquisition, intrusive verification, and perceived security asymmetry to Non-Nuclear Weapon States (NNWS).
% ABSENT_VOICES: Many NNWS, particularly those with acute security concerns, feel their interpretations of Article VI's disarmament obligations are marginalized. States that have withdrawn from the NPT or never joined also represent absent voices, arguing the treaty is discriminatory.
% DISAPPEARANCE_RATIONALE: If the NPT framework, as interpreted by NWS, vanished overnight, it would likely lead to rapid nuclear proliferation, regional arms races, and a fundamental destabilization of global security, as states would pursue nuclear weapons for self-defense without the existing normative and enforcement constraints.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the spread of nuclear weapons beyond the initial five states, while acknowledging their existing arsenals and promoting peaceful nuclear energy.
% FOUNDING_PROBLEM_CORROBORATION: NWS and many international relations scholars attest to the ongoing live problem of proliferation. Many NNWS and disarmament advocates attest that the disarmament problem remains largely unaddressed by NWS, citing the lack of concrete steps and timelines.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because NNWS forgo a strategic capability and accept intrusive verification, while NWS retain their arsenals and face no comparable immediate obligations. Suppression is very high (0.90) due to the robust international enforcement regime, including IAEA safeguards and the threat of UN Security Council sanctions, which effectively limit NNWS's exit options. Theater ratio is low (0.10) because NWS genuinely believe in the necessity of non-proliferation for global stability, and their disarmament commitments, while slow, are not entirely performative from their perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS perspective, the NPT is a successful framework for global stability, with their disarmament being a complex, long-term endeavor. From the NNWS perspective, the same treaty is a discriminatory regime that perpetuates an unfair security asymmetry, with NWS failing to uphold their end of the bargain. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear Weapon States are the primary beneficiaries (d near 0.0) as they maintain their strategic advantage and global influence while benefiting from reduced proliferation risk. Non-Nuclear Weapon States are the primary targets (d near 1.0) as they bear the costs of non-acquisition and intrusive verification. The IAEA and UNSC act as agenda-setters and enforcers, largely aligning with the NWS's interpretation. Global civil society is excluded from direct enforcement but exerts pressure.
 *
 * MANDATROPHY ANALYSIS:
 *   The NWS reading of the NPT has not resolved mandatrophy; rather, it has shifted the perception of the founding problem. While horizontal proliferation remains a live concern, the original mandate for NWS disarmament (Article VI) is widely seen by NNWS as having atrophied into an aspirational goal, leading to persistent tension and accusations of a 'grand bargain' unfulfilled. The constraint persists due to the NWS's institutional power and the perceived necessity of the non-proliferation norm, despite the unresolved asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_interpretation_ambiguity,
    'Is Article VI''s call for NWS disarmament ''at an early date'' a binding, time-sensitive obligation or an aspirational, long-term goal?',
    'A UN General Assembly resolution or ICJ advisory opinion explicitly clarifying the legal force and timeline of Article VI, or NWS adopting concrete, verifiable disarmament timelines.',
    'If binding and time-sensitive, the NWS reading''s extractiveness would be reclassified as higher due to non-compliance, and the NNWS reading would gain legitimacy. If purely aspirational, the NWS reading''s current classification would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_interpretation_ambiguity, conceptual, 'Ambiguity in the legal interpretation of NWS disarmament obligations.').

omega_variable(
    iaea_budget_allocation_bias,
    'Does the IAEA''s safeguards budget allocation disproportionately focus on horizontal proliferation verification (NNWS) compared to vertical proliferation (NWS disarmament verification), reflecting a bias in the NWS reading?',
    'Independent audit of IAEA budget and operational priorities, comparing resources allocated to NNWS safeguards versus any (currently non-existent) NWS disarmament verification activities.',
    'If a significant bias is confirmed, it would provide empirical evidence for the NWS reading''s extractive nature and the institutionalization of the treaty''s asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_budget_allocation_bias, empirical, 'Whether IAEA resource allocation reflects the NWS''s interpretive priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nws_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nws_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nws_reading, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nws_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nws_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nws_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, unsc_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. It focuses on the NWS interpretation, where non-proliferation is binding for NNWS and disarmament is aspirational for NWS. Sibling readings (nnws_reading, withdrawal_threshold_reading) offer alternative interpretations of the treaty's core obligations and exit clauses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
