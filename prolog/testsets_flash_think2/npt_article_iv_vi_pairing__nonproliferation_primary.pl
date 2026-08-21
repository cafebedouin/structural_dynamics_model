% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing (Nonproliferation Primary Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'nonproliferation primary' reading of the
 *   NPT's Article IV/VI pairing. In this reading, the core purpose of the NPT
 *   is to prevent horizontal nuclear proliferation, with Article IV's promise
 *   of peaceful nuclear technology conditional on Article III verification,
 *   and Article VI's disarmament mandate interpreted as aspirational and
 *   non-justiciable. Authority for this interpretation derives from the
 *   security interests of the nuclear weapon states in maintaining a two-tier
 *   nuclear order. The constraint is actively enforced against non-weapon
 *   states, while weapon states' arsenals are largely excluded from its
 *   direct enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.9).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing (Nonproliferation Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, 'f5e9813d-10ec-4f58-a5f1-e1123c19c04a').
narrative_ontology:cs_kernel_codification('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', fixed_text).
narrative_ontology:cs_authority_grounding('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', extraction).
narrative_ontology:cs_interpretation_layer_present('f5e9813d-10ec-4f58-a5f1-e1123c19c04a').
narrative_ontology:cs_reading_relation('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', foundational, weapon_state_nuclear_arsenals_are_legitimate_deterrents).
narrative_ontology:cs_axiom_status(weapon_state_nuclear_arsenals_are_legitimate_deterrents, holdable).
narrative_ontology:cs_axiom_grounding('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', weapon_state_nuclear_arsenals_are_legitimate_deterrents, conventional).
narrative_ontology:cs_reference_frame('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', weapon_state_security_hegemony).
narrative_ontology:cs_drift_state('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f5e9813d-10ec-4f58-a5f1-e1123c19c04a', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_deterrence_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, great_power_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized nuclear weapon states (P5) who interpret the NPT as primarily a non-proliferation instrument. They benefit from the constraint on horizontal proliferation, which secures their strategic interests, while maintaining their own arsenals outside the treaty's enforcement mechanisms. They actively enforce Article IV and III.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, beneficiary).

% States that have foresworn nuclear weapons under the NPT. They bear the burden of inspections and verification (Article III) and forgo a strategic capability, receiving in return a promise of peaceful nuclear technology (Article IV) and a distant aspiration of disarmament (Article VI). Their exit options are severely constrained by international sanctions and military threats.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, beneficiary).

% The International Atomic Energy Agency, responsible for verifying compliance with Article III (safeguards). It acts as the primary enforcement arm for the non-proliferation aspects of the treaty, reporting non-compliance to the UN Security Council, where weapon states hold veto power.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, agenda_setter,
    institutional, generational, constrained, global).

% Non-governmental organizations and advocacy groups that often highlight the perceived hypocrisy of weapon states and advocate for full implementation of Article VI. Their voices are largely excluded from the formal treaty review processes and enforcement mechanisms.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society, excluded,
    powerless, generational, analytical, global).

% The abstract concept of the risk of nuclear weapons spreading. This constraint aims to suppress this risk, thus 'benefiting' from its reduction, though it is not an agent.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_proliferation_risk, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_proliferation_risk).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To prevent the horizontal proliferation of nuclear weapons, thereby stabilizing the international security order and reducing the risk of nuclear war among non-weapon states.
% TRANSFER_FUNCTION: Transfers security benefits (reduced proliferation risk) to weapon states, while transferring the burden of verification, restraint, and strategic disadvantage to non-weapon states. It also transfers peaceful nuclear technology to non-weapon states, conditional on safeguards.
% ABSENT_VOICES: Abolitionist movements and many non-weapon states would argue for the primacy and enforceability of Article VI's disarmament mandate, challenging the permanent two-tier order. They are largely excluded from the decision-making bodies that interpret and enforce the treaty.
% DISAPPEARANCE_RATIONALE: If this interpretation and its enforcement vanished, horizontal proliferation would likely accelerate, leading to a more multipolar and unstable nuclear landscape, potentially increasing the risk of nuclear conflict. The existing international security architecture would be fundamentally reshaped.
% FOUNDING_PROBLEM: To prevent the spread of nuclear weapons beyond the initial five powers, which was seen as an existential threat to global security during the Cold War.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and many security analysts attest that horizontal proliferation remains a live and pressing problem. While some non-weapon states and civil society groups contest the framing, the core concern of preventing more states from acquiring nuclear weapons is widely acknowledged as ongoing.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because non-weapon states bear significant burdens (inspections, forgo strategic options) without a clear reciprocal commitment from weapon states on disarmament. Suppression is very high (0.90) due to the severe international consequences (sanctions, military action) for non-compliance or withdrawal. Theater ratio is low (0.20) because the enforcement of non-proliferation is very real and functional, though the aspirational language of Article VI might be seen as performative. Accessibility collapse is high (0.88) as alternatives to NPT adherence are extremely costly. Resistance is moderate (0.45) as non-weapon states often voice grievances but rarely openly defy Article IV/III.
 *
 * PERSPECTIVAL GAP:
 *   Weapon states perceive this arrangement as a necessary and effective mechanism for global stability, where their security interests align with preventing proliferation. Non-weapon states, particularly those advocating for disarmament, experience it as an unfair, extractive regime that perpetuates a dangerous two-tier order. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states are clear beneficiaries and agenda-setters, as the constraint secures their strategic position and is enforced in line with their security interests (low directionality). Non-weapon states are targets/payers, bearing the costs of restraint and verification (high directionality). The IAEA acts as an institutional enforcer, while global civil society is largely excluded.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforceability_ambiguity,
    'Is Article VI of the NPT genuinely aspirational and non-justiciable, or does it represent a binding legal obligation for nuclear disarmament?',
    'A ruling by the International Court of Justice on the legal enforceability of Article VI, or a new treaty framework that explicitly codifies a disarmament timeline.',
    'If Article VI is deemed binding, the extractiveness from non-weapon states would be re-evaluated downward, and the constraint''s classification would shift towards a more balanced ''rope'' or even ''scaffold'' if a clear disarmament timeline were established. If it remains aspirational, the current ''tangled_rope'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_enforceability_ambiguity, conceptual, 'Ambiguity regarding the legal status and enforceability of NPT Article VI''s disarmament mandate.').

omega_variable(
    weapon_state_security_alignment,
    'Do weapon states'' security interests in preventing horizontal proliferation genuinely align with global security, or do they primarily serve to maintain a strategic advantage?',
    'Empirical analysis of the impact of nuclear arsenals on regional conflicts and global stability, or a shift in weapon states'' policies towards multilateral disarmament initiatives.',
    'If weapon states'' interests are found to diverge significantly from global security, the ''beneficiary'' role of weapon states would be re-evaluated as more purely extractive, increasing the overall extractiveness of the constraint. If alignment is strong, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weapon_state_security_alignment, empirical, 'Whether weapon states'' security interests are truly aligned with broader global security or primarily serve to maintain their strategic advantage.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression on non-weapon states structural (e.g., sanctions, military threats) or internalized (e.g., belief in the legitimacy of the two-tier order)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., sanctions lifted, threats receded), reclassify as partially internalized. Analysis of national security discourse in non-weapon states.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more robust. If purely structural, removing external pressures would lead to faster shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-weapon states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1970, 0.7).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(npt__su_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Article IV/VI pairing kernel, focusing on nonproliferation as the primary goal. It is structurally distinct from the 'grand_bargain' and 'abolitionist' readings, which emphasize reciprocity and disarmament respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
