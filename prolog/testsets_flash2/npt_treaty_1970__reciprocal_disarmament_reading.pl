% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Treaty (1970): Reciprocal Disarmament Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'reciprocal disarmament' reading
 *   of the NPT, emphasizing Article VI as a binding legal obligation with
 *   temporal urgency. It frames horizontal and vertical nonproliferation as a
 *   reciprocal bargain, where the non-nuclear-weapon states' (NNWS)
 *   commitment to forgo nuclear weapons is contingent on the nuclear-weapon
 *   states' (NWS) good-faith pursuit of disarmament. This reading highlights
 *   the NWS's strategic autonomy as a victim of the constraint, and the NNWS
 *   coalition as a beneficiary gaining normative leverage. The enforcement
 *   gap for Article VI is seen as a structural injustice, not merely an
 *   implementation detail.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.75).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Treaty (1970): Reciprocal Disarmament Reading").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, 'e6ba05ca-15e8-46a8-a4a4-9d9868c94902').
narrative_ontology:cs_kernel_codification('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', fixed_text).
narrative_ontology:cs_authority_grounding('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', lineage).
narrative_ontology:cs_interpretation_layer_present('e6ba05ca-15e8-46a8-a4a4-9d9868c94902').
narrative_ontology:cs_reading_relation('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', foundational, article_vi_binding_and_urgent).
narrative_ontology:cs_axiom_status(article_vi_binding_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', article_vi_binding_and_urgent, deontological).
narrative_ontology:cs_axiom('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', foundational, horizontal_and_vertical_nonproliferation_reciprocal).
narrative_ontology:cs_axiom_status(horizontal_and_vertical_nonproliferation_reciprocal, holdable).
narrative_ontology:cs_axiom_grounding('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', horizontal_and_vertical_nonproliferation_reciprocal, conventional).
narrative_ontology:cs_reference_frame('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', original_reciprocal_bargain).
narrative_ontology:cs_drift_state('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6ba05ca-15e8-46a8-a4a4-9d9868c94902', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, global_security_regime).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, aspiring_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As signatories to Article VI, they are legally obligated to pursue nuclear disarmament in good faith. This reading places their strategic autonomy and modernization programs under direct constraint, making them both enforcers of horizontal nonproliferation and targets of vertical nonproliferation demands. Their exit is constrained by global normative pressure and the risk of regime collapse.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer).

% These states forgo nuclear weapons in exchange for security assurances and the NWS commitment to disarm. This reading grants them significant normative leverage to demand NWS compliance with Article VI, framing their nonproliferation as a reciprocal bargain. Their exit is constrained by the security implications of developing nuclear weapons.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, beneficiary,
    organized, generational, constrained, global).

% The IAEA verifies compliance with Articles I and II (horizontal nonproliferation) but has no mandate to verify Article VI disarmament efforts. In this reading, the absence of Article VI verification is a structural injustice, highlighting the regime's asymmetry. The IAEA's role is to monitor and report, but its mandate is limited by the NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the reduction of nuclear proliferation risk, both horizontal and vertical. The NPT is a cornerstone of this regime, and its integrity is essential for global stability. Its 'exit' would be a catastrophic breakdown of international order.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, global_security_regime, beneficiary,
    institutional, civilizational, identity_locked, universal).

% States that seek nuclear weapons outside the NPT framework. They are excluded from the benefits of the regime and face international sanctions and pressure. This reading views their aspirations as a direct challenge to the reciprocal bargain, but also as a symptom of NWS non-compliance with Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, aspiring_nuclear_weapon_states, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to prevent the spread of nuclear weapons (horizontal nonproliferation) and commits nuclear-weapon states to disarmament (vertical nonproliferation), establishing a reciprocal bargain for collective security.
% TRANSFER_FUNCTION: Transfers the right to develop nuclear weapons from non-nuclear-weapon states to nuclear-weapon states, in exchange for security assurances and a commitment to disarm. It also transfers normative authority to NNWS to demand NWS disarmament.
% ABSENT_VOICES: States that have developed nuclear weapons outside the NPT (e.g., India, Pakistan, Israel, North Korea) are absent from the core bargain, as are those who advocate for a more rapid, verifiable disarmament timeline for NWS. They would argue the NPT is inherently discriminatory and that NWS have no intention of disarming.
% DISAPPEARANCE_RATIONALE: If the NPT vanished, the reciprocal bargain would collapse. Non-nuclear-weapon states would face immense pressure to develop their own nuclear deterrents, leading to a rapid increase in horizontal proliferation. Nuclear-weapon states would lose a key legitimizing framework for their arsenals, and the global security landscape would become far more unstable.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the proliferation of nuclear weapons to more states, while acknowledging the existing nuclear arsenals of a few powers.
% FOUNDING_PROBLEM_CORROBORATION: The non-nuclear-weapon states coalition consistently attests that the founding problem of nuclear proliferation remains live, and that NWS non-compliance with Article VI exacerbates this problem. UN resolutions and numerous international conferences corroborate the ongoing threat and the need for disarmament.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) reflects the ongoing cost to NNWS of forgoing nuclear weapons without commensurate NWS disarmament, and the constraint on NWS strategic autonomy. Suppression (0.75) is high due to the international pressure and sanctions against aspiring nuclear states, and the diplomatic pressure on NWS to maintain the facade of disarmament efforts. The theater ratio (0.45) indicates that while some disarmament efforts are genuine, a significant portion of NWS activity is performative, designed to maintain the regime's legitimacy without deep structural change. Resistance (0.8) is high, primarily from NNWS demanding greater NWS compliance and from aspiring nuclear states challenging the regime's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS perspective, the NPT is primarily about horizontal nonproliferation, with Article VI being aspirational. From the NNWS perspective, Article VI is a binding, urgent obligation, and the NWS's failure to disarm undermines the entire reciprocal bargain. This reading emphasizes the NNWS perspective, where the NWS's strategic autonomy is a cost, and the lack of Article VI verification is a structural flaw.
 *
 * DIRECTIONALITY LOGIC:
 *   The NNWS coalition and the global security regime are beneficiaries, gaining from reduced horizontal proliferation and the normative framework for disarmament. The NWS are both agenda-setters (enforcing horizontal nonproliferation) and payers/victims (constrained in their vertical proliferation and strategic autonomy by Article VI). Aspiring nuclear states are excluded and targeted by the regime's enforcement mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the NPT as pure coordination by highlighting the asymmetric extraction from NNWS and the NWS's resistance to full Article VI compliance. It frames the persistence of NWS arsenals not as a natural outcome, but as a failure of the reciprocal bargain, sustained by active enforcement against horizontal proliferation without equivalent enforcement of vertical disarmament. The 'live' status of the founding problem, coupled with the 'world_rearranges' disappearance verdict, suggests the constraint is still functional but deeply contested in its current form, preventing a 'piton' classification despite the high theater ratio.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_identity,
    'Is this constraint a genuine ''reciprocal disarmament'' reading of the NPT, or is it an ''oligopoly enforcement'' reading with a strong normative critique?',
    'Analysis of state practice and diplomatic discourse: if NWS consistently frame Article VI as aspirational and resist verification, it leans towards oligopoly enforcement. If NNWS consistently frame their nonproliferation as contingent on NWS disarmament, it supports the reciprocal disarmament reading.',
    'If it''s primarily an oligopoly enforcement reading, the extractiveness from NNWS is higher, and the claimed type might shift towards Snare. If it''s a strong reciprocal disarmament reading, the NWS are more clearly victims of their own commitment, and the NNWS have greater normative power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_identity, conceptual, 'Distinguishing between a genuine reciprocal disarmament framework and a critique of an oligopoly enforcement framework.').

omega_variable(
    article_vi_verifiability,
    'Is Article VI (disarmament) genuinely verifiable with existing or foreseeable international mechanisms, or is its non-verification a structural feature of nuclear deterrence?',
    'Technical assessment by disarmament experts on the feasibility of verifying deep cuts and elimination of nuclear arsenals, coupled with political will analysis.',
    'If verifiable, the lack of verification mechanisms is a political choice by NWS, increasing the perceived extraction and suppression. If non-verifiable, the constraint''s ''reciprocal bargain'' aspect is fundamentally flawed, potentially shifting the claimed type towards Snare due to an unfulfillable promise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_verifiability, empirical, 'The technical and political feasibility of verifying nuclear disarmament.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the NPT Treaty (1970) kernel. This 'reciprocal disarmament' reading emphasizes Article VI as a binding, urgent obligation, contrasting with the 'oligopoly enforcement' reading (Articles I-II primary) and the 'withdrawal sovereignty' reading (Article X primary). Each reading instantiates a structurally distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
