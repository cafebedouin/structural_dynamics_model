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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Grand Bargain: Article IV/VI Reciprocity
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'grand bargain' reading of the
 *   Nuclear Non-Proliferation Treaty (NPT), which posits that Article IV
 *   (peaceful nuclear energy) and Article VI (disarmament) are reciprocal
 *   obligations. Under this reading, non-nuclear weapon states' (NNWS)
 *   restraint from acquiring nuclear weapons is conditional on weapon states'
 *   (WWS) progress towards disarmament. A breach of Article VI by WWS is seen
 *   as undermining the legitimacy of Article IV and the overall
 *   non-proliferation regime. This reading emphasizes the enforceability of
 *   the disarmament obligation and views WWS as potential treaty-breach
 *   actors.
 *
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
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain: Article IV/VI Reciprocity").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '89f55f28-5fc4-4e92-b95d-5b712ae98f95').
narrative_ontology:cs_kernel_codification('89f55f28-5fc4-4e92-b95d-5b712ae98f95', fixed_text).
narrative_ontology:cs_authority_grounding('89f55f28-5fc4-4e92-b95d-5b712ae98f95', lineage).
narrative_ontology:cs_interpretation_layer_present('89f55f28-5fc4-4e92-b95d-5b712ae98f95').
narrative_ontology:cs_reading_relation('89f55f28-5fc4-4e92-b95d-5b712ae98f95', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('89f55f28-5fc4-4e92-b95d-5b712ae98f95', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('89f55f28-5fc4-4e92-b95d-5b712ae98f95', foundational, disarmament_is_enforceable_obligation).
narrative_ontology:cs_axiom_status(disarmament_is_enforceable_obligation, holdable).
narrative_ontology:cs_axiom_grounding('89f55f28-5fc4-4e92-b95d-5b712ae98f95', disarmament_is_enforceable_obligation, conventional).
narrative_ontology:cs_axiom('89f55f28-5fc4-4e92-b95d-5b712ae98f95', foundational, nnws_restraint_is_conditional).
narrative_ontology:cs_axiom_status(nnws_restraint_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('89f55f28-5fc4-4e92-b95d-5b712ae98f95', nnws_restraint_is_conditional, conventional).
narrative_ontology:cs_reference_frame('89f55f28-5fc4-4e92-b95d-5b712ae98f95', original_npt_bargain_framework).
narrative_ontology:cs_drift_state('89f55f28-5fc4-4e92-b95d-5b712ae98f95', contemporary_disarmament_stalemate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89f55f28-5fc4-4e92-b95d-5b712ae98f95', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, international_community).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, international_treaty_law).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, collective_security_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the non-proliferation commitments of NNWS, but are obligated by Article VI to pursue disarmament. Their perceived security interests often conflict with their disarmament obligations, leading to slow progress. Withdrawal from the NPT would incur significant diplomatic and security costs.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, weapon_states, beneficiary).

% Commit to not acquiring nuclear weapons (Article II) and accept safeguards (Article III), in exchange for access to peaceful nuclear technology (Article IV) and the promise of disarmament by WWS (Article VI). They bear the cost of restraint and feel extracted from when disarmament lags. Withdrawal is a high-cost option, often leading to international isolation and sanctions.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states, beneficiary).

% Responsible for verifying NNWS compliance with Article III safeguards, ensuring nuclear material is not diverted to weapons programs. Plays a crucial role in the NPT's non-proliferation function, but has no direct enforcement power over WWS disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, iaea, observer).

% Benefits from the reduced risk of nuclear war and proliferation that the NPT aims to achieve. Exerts diplomatic pressure on both WWS and NNWS to uphold their treaty obligations.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_community, beneficiary,
    organized, civilizational, mobile, global).

% States that have developed nuclear weapons outside the NPT framework or have withdrawn from it. They are excluded from the NPT regime's benefits and obligations, and their existence challenges the treaty's universality and legitimacy, particularly from the perspective of NNWS.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_proliferation_states, excluded,
    powerful, biographical, arbitrage, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to prevent the spread of nuclear weapons, facilitate peaceful nuclear energy use, and pursue nuclear disarmament, by establishing reciprocal obligations between nuclear and non-nuclear weapon states.
% TRANSFER_FUNCTION: Transfers security assurances and access to peaceful nuclear technology to non-nuclear weapon states, in exchange for their commitment to non-proliferation, conditional on nuclear weapon states' progress towards disarmament.
% ABSENT_VOICES: States that have proliferated outside the NPT (e.g., India, Pakistan, Israel, North Korea) are structurally excluded from the NPT's framework. They would argue for their sovereign right to nuclear weapons or against the NPT's discriminatory nature, but their voices are not formally integrated into the NPT review process.
% DISAPPEARANCE_RATIONALE: The NPT is a foundational pillar of global security. Its disappearance would likely lead to widespread nuclear proliferation, a breakdown of international arms control efforts, and a dramatic increase in global instability and the risk of nuclear conflict, fundamentally reorganizing the international security landscape.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the imperative to prevent further horizontal proliferation of nuclear weapons after the initial arms race, while ensuring access to peaceful nuclear technology.
% FOUNDING_PROBLEM_CORROBORATION: UN resolutions, NPT Review Conference documents, statements from non-weapon states, and independent international security analyses consistently corroborate the ongoing relevance of nuclear proliferation risks and the unfulfilled disarmament mandate, supporting the 'live' status of the founding problem.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The 'grand bargain' reading leads to a high extractiveness score (0.65) because NNWS bear the ongoing cost of non-acquisition without the promised reciprocal benefit of WWS disarmament. Suppression (0.70) is high due to the robust international regime (IAEA safeguards, sanctions) preventing NNWS proliferation. Theater ratio (0.40) reflects the performative nature of some disarmament negotiations that yield little concrete progress. Accessibility collapse (0.75) is high for NNWS, as developing nuclear weapons is severely constrained. Resistance (0.60) is moderate, reflecting persistent calls from NNWS for WWS to fulfill their Article VI obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of many NNWS, the NPT is an unfulfilled grand bargain, where their restraint is met with insufficient disarmament progress from WWS. From the perspective of WWS, the NPT is primarily a non-proliferation regime, with disarmament as a long-term aspiration, not a strict condition for NNWS compliance. This divergence in interpretation is central to the constraint's contested nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, WWS are beneficiaries of NNWS non-proliferation but also payers of the disarmament obligation. NNWS are payers of non-proliferation but beneficiaries of peaceful nuclear technology and the disarmament promise. However, the lagging disarmament makes NNWS net targets of extraction. The IAEA acts as an agenda-setter for verification and an observer of the regime. Nuclear proliferation states are excluded, their existence challenging the regime's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to achieve disarmament (Article VI) is widely perceived as unfulfilled, leading to a state of 'contested' founding problem status. While the non-proliferation aspect (Article IV) remains live, the imbalance creates pressure for mandatrophy. The high extractiveness and resistance from NNWS indicate that the constraint is not merely inertial (Piton) but actively extractive (Tangled Rope) due to the unfulfilled reciprocal obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disarmament_verifiability_ambiguity,
    'Is WWS disarmament genuinely verifiable in a manner that satisfies NNWS security concerns and builds trust?',
    'Development and implementation of robust, intrusive, and transparent verification mechanisms for WWS disarmament, accepted by all parties.',
    'If verifiable, NNWS trust in the grand bargain would increase, potentially reducing extractiveness and resistance. If not, NNWS would continue to perceive the disarmament obligation as non-justiciable, exacerbating extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_verifiability_ambiguity, empirical, 'Uncertainty regarding the technical and political feasibility of WWS disarmament verification.').

omega_variable(
    reciprocity_enforceability_ambiguity,
    'How enforceable is the reciprocity clause for NNWS, allowing them to adjust their commitments if WWS fail to disarm?',
    'International legal rulings or state practice establishing clear precedents for NNWS withdrawal or re-evaluation of Article IV commitments in response to WWS Article VI breaches.',
    'If enforceable, NNWS would have stronger leverage, potentially rebalancing the extraction. If not, the grand bargain remains largely one-sided, reinforcing the current extractive dynamics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_enforceability_ambiguity, conceptual, 'Ambiguity regarding the legal and political mechanisms for NNWS to enforce the reciprocal nature of the NPT.').

omega_variable(
    grand_bargain_legitimacy_erosion,
    'To what extent has the legitimacy of the ''grand bargain'' reading eroded among NNWS due to persistent WWS disarmament delays?',
    'Surveys of NNWS diplomatic positions, analysis of NPT Review Conference statements, and voting patterns on disarmament resolutions.',
    'Significant erosion of legitimacy would increase the risk of NNWS withdrawal or pursuit of alternative security arrangements (e.g., TPNW), potentially leading to a breakdown of the NPT regime. Sustained legitimacy would maintain the current, albeit strained, framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grand_bargain_legitimacy_erosion, empirical, 'Assessment of the NPT''s perceived legitimacy among non-nuclear weapon states.').


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
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1980, 0.45).
narrative_ontology:measurement(npt__be_t1990, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.62).
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
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapons_prohibition_treaty).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, iaea_safeguards_regime).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('grand_bargain') of the NPT Article IV/VI pairing kernel. Other readings include 'nonproliferation_primary' (emphasizing WWS security interests and aspirational disarmament) and 'abolitionist' (emphasizing complete disarmament and humanitarian law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
