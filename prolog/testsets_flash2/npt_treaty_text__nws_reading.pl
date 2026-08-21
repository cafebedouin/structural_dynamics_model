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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Treaty Text (NWS Reading): Non-Proliferation as Binding Constraint
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the Nuclear Non-Proliferation Treaty (NPT) as
 *   interpreted by Nuclear Weapon States (NWS). In this reading,
 *   non-proliferation is a binding, enforceable obligation for Non-Nuclear
 *   Weapon States (NNWS), while disarmament (Article VI) is an aspirational,
 *   long-term goal without specific timelines or enforcement. This
 *   interpretation allows NWS to maintain their arsenals while strictly
 *   controlling horizontal proliferation. The high extractiveness reflects
 *   the asymmetric burden on NNWS, and the rising theater ratio indicates the
 *   increasing performative nature of disarmament commitments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Treaty Text (NWS Reading): Non-Proliferation as Binding Constraint").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'e191ffc5-c4b9-490d-834e-f749c7f3da46').
narrative_ontology:cs_kernel_codification('e191ffc5-c4b9-490d-834e-f749c7f3da46', fixed_text).
narrative_ontology:cs_authority_grounding('e191ffc5-c4b9-490d-834e-f749c7f3da46', extraction).
narrative_ontology:cs_interpretation_layer_present('e191ffc5-c4b9-490d-834e-f749c7f3da46').
narrative_ontology:cs_reading_relation('e191ffc5-c4b9-490d-834e-f749c7f3da46', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('e191ffc5-c4b9-490d-834e-f749c7f3da46', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('e191ffc5-c4b9-490d-834e-f749c7f3da46', foundational, non_proliferation_is_binding_obligation).
narrative_ontology:cs_axiom_status(non_proliferation_is_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('e191ffc5-c4b9-490d-834e-f749c7f3da46', non_proliferation_is_binding_obligation, conventional).
narrative_ontology:cs_axiom('e191ffc5-c4b9-490d-834e-f749c7f3da46', foundational, disarmament_is_aspirational_goal).
narrative_ontology:cs_axiom_status(disarmament_is_aspirational_goal, holdable).
narrative_ontology:cs_axiom_grounding('e191ffc5-c4b9-490d-834e-f749c7f3da46', disarmament_is_aspirational_goal, conventional).
narrative_ontology:cs_reference_frame('e191ffc5-c4b9-490d-834e-f749c7f3da46', nws_security_paradigm).
narrative_ontology:cs_drift_state('e191ffc5-c4b9-490d-834e-f749c7f3da46', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e191ffc5-c4b9-490d-834e-f749c7f3da46', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Article VI's disarmament obligation as an aspirational long-term goal without specific enforcement mechanisms or timelines, while enforcing non-proliferation strictly on NNWS. They benefit from maintaining their nuclear arsenals and the associated geopolitical leverage.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Are bound by strict non-proliferation commitments, including IAEA safeguards, without a reciprocal, enforceable commitment from NWS to disarm. They bear the cost of foregoing nuclear weapons development, often perceiving an imbalance in treaty obligations.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% Receives its mandate and funding primarily for verifying non-proliferation in NNWS, aligning with the NWS reading. Its budget and operational focus are heavily weighted towards horizontal proliferation, reinforcing the NWS interpretation of the treaty's priorities.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea, agenda_setter).

% Advocates for universal nuclear disarmament and a more balanced interpretation of the NPT, but lacks direct enforcement power within the treaty framework. Their voice is often marginalized in state-centric discussions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, global_civil_society, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global norm against the spread of nuclear weapons, aiming to prevent horizontal proliferation and maintain international stability by limiting the number of nuclear-armed states.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons (and the associated security/deterrence benefits) from NNWS to NWS, in exchange for a promise of eventual disarmament by NWS and security assurances for NNWS.
% ABSENT_VOICES: Many non-nuclear weapon states, particularly those in contested regions, would argue for a more robust and time-bound disarmament commitment from NWS. Global civil society organizations also advocate for a stronger disarmament agenda.
% DISAPPEARANCE_RATIONALE: If the NPT vanished overnight, the global non-proliferation regime would collapse, likely leading to a rapid increase in nuclear weapon states as NNWS pursue their own deterrents, fundamentally altering the international security landscape.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent an uncontrolled spread of nuclear weapons to more states, following the Cuban Missile Crisis and early proliferation concerns.
% FOUNDING_PROBLEM_CORROBORATION: The NWS attest the problem is live, citing ongoing proliferation risks. Many NNWS and independent analysts corroborate the original problem but argue the NWS reading has exacerbated the imbalance, creating new risks.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high due to the indefinite retention of nuclear weapons by NWS and the lack of progress on disarmament, while NNWS face strict verification. Suppression (0.85) is very high, reflecting the severe consequences for NNWS attempting to develop nuclear weapons, enforced by international sanctions and military threats. The theater ratio (0.60) is substantial and rising, as NWS engage in disarmament rhetoric and limited arms control agreements that do not fundamentally challenge their nuclear status, while the core obligation remains unfulfilled. The claimed type is 'tangled_rope' because it genuinely coordinates non-proliferation (a collective good) but does so with significant asymmetric extraction from NNWS.
 *
 * PERSPECTIVAL GAP:
 *   NNWS experience this constraint as highly extractive and suppressive, a 'snare' that locks them into a disadvantageous security position. NWS, however, perceive it as a 'rope' that maintains global stability and prevents dangerous proliferation. The engine's classification will highlight this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear Weapon States are the primary beneficiaries and agenda-setters, shaping the treaty's interpretation to their advantage (low directionality). Non-Nuclear Weapon States are the primary payers/victims, bearing the costs of non-proliferation without reciprocal disarmament (high directionality). The IAEA benefits from its mandate to verify non-proliferation, aligning its operational focus with the NWS reading. Global civil society is excluded from direct influence, despite advocating for a more balanced interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The NWS reading prevents mislabeling the non-proliferation function as pure extraction by acknowledging the genuine coordination problem it solves (preventing horizontal proliferation). However, the high extractiveness and theater ratio indicate that the disarmament mandate has atrophied, transforming the constraint into a 'tangled rope' where the coordination function is increasingly overshadowed by the extractive asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disarmament_timeline_ambiguity,
    'Is Article VI''s ''at an early date'' a genuinely ambiguous phrase, or is its ambiguity strategically maintained by NWS to avoid concrete disarmament obligations?',
    'Analysis of NWS internal policy documents and diplomatic communications over time, seeking evidence of deliberate interpretive strategies to defer disarmament.',
    'If strategically maintained, the extractiveness of the NWS reading is higher, as the ambiguity itself becomes an extractive mechanism. If genuinely ambiguous, the constraint''s inherent complexity is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_timeline_ambiguity, conceptual, 'Ambiguity of Article VI''s disarmament timeline.').

omega_variable(
    iaea_mandate_bias,
    'To what extent does the IAEA''s operational mandate and funding structure inherently bias its focus towards horizontal proliferation verification over vertical disarmament verification?',
    'Independent audit of IAEA budget allocations, staffing, and operational priorities, comparing resources dedicated to NNWS safeguards versus NWS disarmament verification (if any).',
    'If a strong bias is confirmed, the IAEA''s role as a ''beneficiary'' of the NWS reading is reinforced, and its perceived neutrality as an ''observer'' is diminished, increasing the overall extractiveness of the NWS-driven regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(iaea_mandate_bias, empirical, 'IAEA''s focus on horizontal vs. vertical proliferation.').

omega_variable(
    npt_legitimacy_erosion,
    'Is the NWS reading of the NPT causing a long-term erosion of the treaty''s legitimacy among NNWS, increasing the risk of withdrawals or non-compliance?',
    'Longitudinal analysis of NNWS statements, voting patterns in international fora, and adherence to additional protocols, correlated with NWS disarmament progress (or lack thereof).',
    'If legitimacy erosion is substantial, the long-term stability of the non-proliferation regime is threatened, potentially leading to a ''snare'' outcome for the entire system if NNWS perceive the costs of adherence to outweigh the benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_legitimacy_erosion, empirical, 'Impact of NWS reading on NPT legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__nws_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nws_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__nws_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nws_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__nws_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nws_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. The NWS reading emphasizes non-proliferation as binding for NNWS, while treating disarmament as aspirational. This contrasts with the NNWS reading, which views disarmament as a binding obligation, and the withdrawal threshold reading, which focuses on Article X conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
