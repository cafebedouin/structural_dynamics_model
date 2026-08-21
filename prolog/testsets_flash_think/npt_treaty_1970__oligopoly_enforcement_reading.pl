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
 *   human_readable: NPT (1970) - Oligopoly Enforcement Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint is the 'oligopoly enforcement' reading of the NPT kernel,
 *   focusing on Articles I-II (horizontal proliferation prevention) as
 *   primary binding obligations, while viewing Article VI's disarmament
 *   commitments as contingent and aspirational. This reading emphasizes the
 *   NPT's role in maintaining a nuclear status quo where a few states retain
 *   weapons and others are denied them. Sibling readings include the
 *   'reciprocal disarmament' reading (emphasizing Article VI's binding
 *   nature) and the 'withdrawal sovereignty' reading (emphasizing Article X's
 *   legitimacy).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.87).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.9).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT (1970) - Oligopoly Enforcement Reading").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'fd7e7a40-ccd5-479e-b42c-1b6c5660e439').
narrative_ontology:cs_kernel_codification('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', fixed_text).
narrative_ontology:cs_authority_grounding('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', extraction).
narrative_ontology:cs_interpretation_layer_present('fd7e7a40-ccd5-479e-b42c-1b6c5660e439').
narrative_ontology:cs_reading_relation('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', foundational, nws_status_is_legitimate_security_arrangement).
narrative_ontology:cs_axiom_status(nws_status_is_legitimate_security_arrangement, holdable).
narrative_ontology:cs_axiom_grounding('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', nws_status_is_legitimate_security_arrangement, conventional).
narrative_ontology:cs_reference_frame('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', stable_nuclear_oligopoly).
narrative_ontology:cs_drift_state('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', contemporary_proliferation_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd7e7a40-ccd5-479e-b42c-1b6c5660e439', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, un_security_council).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear-weapon states (P5) under the NPT. They retain their nuclear arsenals, control the non-proliferation regime's enforcement mechanisms (e.g., UNSC veto), and benefit from the status hierarchy that denies nuclear weapons to others. Their commitment to Article VI disarmament is largely aspirational under this reading.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% States that have foresworn nuclear weapons under the NPT. They are subject to IAEA safeguards and inspections, bear the costs of non-proliferation, and are denied the option of developing a nuclear deterrent. Their security is theoretically guaranteed by NWS, but this is often perceived as insufficient.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states, payer,
    organized, biographical, constrained, global).

% The International Atomic Energy Agency, responsible for verifying NNWS compliance with their non-proliferation obligations through safeguards. Its mandate and institutional power are strengthened by the NPT regime, even as it operates under the political influence of NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea, agenda_setter,
    institutional, biographical, analytical, global).

% Non-nuclear-weapon states with the technical capability or strategic incentive to develop nuclear weapons, but which are constrained by the NPT regime and international pressure. They are often targets of intense scrutiny and sanctions, effectively denied a deterrent option available to NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    moderate, biographical, trapped, regional).

% The primary enforcement body for the NPT, dominated by the P5. It can impose sanctions or authorize military action against states violating non-proliferation norms, effectively upholding the NWS oligopoly.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% NGOs, academics, and civil society groups that monitor the NPT's implementation and advocate for universal disarmament. They often highlight the asymmetry of the regime and the lack of NWS accountability for Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, global_nonproliferation_advocates, observer,
    organized, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the horizontal proliferation of nuclear weapons, aiming to create a more stable international security environment by limiting the number of actors with such weapons and providing a framework for peaceful nuclear cooperation.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons from non-nuclear-weapon states to the five recognized nuclear-weapon states, in exchange for security assurances and access to peaceful nuclear technology (which is often constrained).
% ABSENT_VOICES: States denied a nuclear deterrent (e.g., Iran, North Korea, though NK withdrew) and those advocating for universal disarmament (e.g., signatories of the Treaty on the Prohibition of Nuclear Weapons) are structurally excluded from setting the terms of the NPT regime, which prioritizes the NWS oligopoly.
% DISAPPEARANCE_RATIONALE: If the NPT and its enforcement mechanisms vanished overnight, many non-nuclear-weapon states would likely pursue nuclear weapons programs, leading to a rapid and dangerous proliferation cascade, fundamentally altering global security and increasing the risk of nuclear conflict.
% FOUNDING_PROBLEM: The existential threat of widespread nuclear proliferation in the Cold War era, where many states had the capability to develop nuclear weapons, risking global catastrophe and destabilizing international relations.
% FOUNDING_PROBLEM_CORROBORATION: The NPT's founding problem is widely corroborated by international security experts, historians, and non-proliferation organizations (e.g., UNODA, Arms Control Association), who consistently warn of ongoing proliferation risks in its absence, despite the regime's asymmetries.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.87, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.87) reflects the denial of nuclear weapons to NNWS, which is a significant strategic cost. Suppression (0.90) is severe due to the robust enforcement mechanisms (IAEA, UNSC sanctions, potential military action) against NNWS non-compliance, coupled with the lack of similar accountability for NWS. The high theater ratio (0.70) indicates that NWS commitments to disarmament under Article VI are largely performative, with little genuine progress, while the regime's primary function remains horizontal non-proliferation. Accessibility collapse is high for NNWS as they are denied a key security option.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of NWS, this reading of the NPT is a necessary and effective mechanism for global stability, preventing a dangerous free-for-all. From the perspective of NNWS and threshold states, it is an extractive regime that perpetuates an unjust nuclear oligopoly, denying them sovereign security options while NWS fail to uphold their end of the bargain. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear-weapon states, the IAEA, and the UNSC are beneficiaries and agenda-setters, as they control and benefit from the regime's structure. Non-nuclear-weapon states and threshold states are payers and victims, bearing the costs of non-proliferation without the reciprocal disarmament promised by Article VI. Global non-proliferation advocates act as observers, often critiquing the regime's asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing proliferation) is still live, but its *form* has drifted. The original bargain (NNWS forgo weapons in exchange for NWS disarmament) has atrophied on the NWS side, leading to a higher extractiveness and theater ratio. The classification as a Tangled Rope, rather than a Rope, correctly identifies the asymmetric extraction layered onto a genuine coordination function, preventing mislabeling it as purely beneficial coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_nature,
    'Is NPT Article VI (disarmament) a legally binding obligation with temporal urgency, or an aspirational goal?',
    'International Court of Justice advisory opinion on the legal status of Article VI, or a new NPT Review Conference outcome explicitly clarifying its binding nature and timeline.',
    'If legally binding, the NWS''s non-compliance would be a direct violation, increasing the regime''s extractiveness and suppression from the NNWS perspective. If aspirational, the current high theater ratio is more structurally inherent to this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_nature, conceptual, 'Ambiguity of Article VI''s legal force.').

omega_variable(
    oligopoly_legitimacy,
    'Is the nuclear-weapon states'' oligopoly a legitimate and stable security arrangement, or a constructed power asymmetry that breeds instability?',
    'Empirical analysis of regional proliferation dynamics in the absence of NWS disarmament, or a shift in international norms regarding nuclear deterrence.',
    'If legitimate, the high extractiveness is a necessary cost of global stability. If constructed and unstable, the regime''s long-term viability is questionable, and its classification as a Tangled Rope is further reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_legitimacy, empirical, 'Legitimacy of the NWS nuclear status.').

omega_variable(
    horizontal_vs_vertical_proliferation,
    'Does the NPT''s success in preventing horizontal proliferation outweigh the risks posed by unchecked vertical proliferation (NWS modernization)?',
    'Comprehensive risk assessment comparing the likelihood and impact of new states acquiring nuclear weapons versus the risks of NWS developing new, more destabilizing capabilities.',
    'If vertical proliferation risks are deemed higher, the regime''s current focus on horizontal non-proliferation is misaligned, potentially increasing the effective extraction from NNWS who bear the costs without reduced overall risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(horizontal_vs_vertical_proliferation, empirical, 'Relative importance of horizontal vs. vertical proliferation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2020, 0.65).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.7).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2020, 0.85).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.85).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2020, 0.88).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, unsc_sanctions_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT (1970) kernel, focusing on the enforcement of horizontal non-proliferation and the maintenance of the NWS oligopoly. It is linked to sibling readings that emphasize reciprocal disarmament and withdrawal sovereignty, as well as to the operational regimes it relies upon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
