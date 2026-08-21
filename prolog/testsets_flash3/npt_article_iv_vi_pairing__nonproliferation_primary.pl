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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing: Nonproliferation Primary Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'nonproliferation primary' reading of the
 *   NPT's Article IV (peaceful nuclear energy) and Article VI (disarmament)
 *   pairing. In this reading, Article IV's permission for peaceful nuclear
 *   technology is strictly conditional on Article III verification, while
 *   Article VI's disarmament mandate is treated as aspirational and
 *   non-justiciable. Authority for the treaty derives from the security
 *   interests of nuclear weapon states in preventing horizontal
 *   proliferation, effectively stabilizing a two-tier nuclear order where
 *   non-weapon states bear perpetual restraint. This reading is a Snare, as
 *   it extracts heavily from non-nuclear weapon states and suppresses their
 *   security alternatives, while providing substantial benefits to nuclear
 *   weapon states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.92).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing: Nonproliferation Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '9c4c9549-8088-40e4-bf85-6b72abed2427').
narrative_ontology:cs_kernel_codification('9c4c9549-8088-40e4-bf85-6b72abed2427', fixed_text).
narrative_ontology:cs_authority_grounding('9c4c9549-8088-40e4-bf85-6b72abed2427', extraction).
narrative_ontology:cs_interpretation_layer_present('9c4c9549-8088-40e4-bf85-6b72abed2427').
narrative_ontology:cs_reading_relation('9c4c9549-8088-40e4-bf85-6b72abed2427', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('9c4c9549-8088-40e4-bf85-6b72abed2427', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('9c4c9549-8088-40e4-bf85-6b72abed2427', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('9c4c9549-8088-40e4-bf85-6b72abed2427', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('9c4c9549-8088-40e4-bf85-6b72abed2427', foundational, nuclear_weapon_state_security_interests_are_paramount).
narrative_ontology:cs_axiom_status(nuclear_weapon_state_security_interests_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('9c4c9549-8088-40e4-bf85-6b72abed2427', nuclear_weapon_state_security_interests_are_paramount, conventional).
narrative_ontology:cs_reference_frame('9c4c9549-8088-40e4-bf85-6b72abed2427', stable_two_tier_nuclear_order).
narrative_ontology:cs_drift_state('9c4c9549-8088-40e4-bf85-6b72abed2427', contemporary, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('9c4c9549-8088-40e4-bf85-6b72abed2427', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret the NPT as primarily a nonproliferation instrument, where their security interests in preventing horizontal proliferation justify the indefinite retention of their own arsenals. They enforce Article III verification and Article IV compliance, while treating Article VI disarmament obligations as aspirational and non-justiciable. They benefit from a stabilized two-tier nuclear order.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states bear the primary burden of nonproliferation, submitting to IAEA safeguards under Article III and foregoing nuclear weapons development under Article IV. They experience the constraint as a permanent limitation on their sovereignty and security options, with no clear reciprocal disarmament from weapon states. Their exit options are limited by international pressure and potential sanctions.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% The International Atomic Energy Agency is tasked with verifying nonproliferation commitments under Article III. Its mandate and operational scope are heavily influenced by the nuclear weapon states, making it an enforcer of the nonproliferation primary reading, even as its technical mission is neutral. It collects resources for verification but operates within the framework set by weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, agenda_setter,
    institutional, generational, constrained, global).

% Advocates for nuclear disarmament and a world free of nuclear weapons. They are largely excluded from the formal interpretive processes of the NPT, viewing the nonproliferation primary reading as a perpetuation of an unjust and dangerous status quo. Their resistance is primarily through advocacy and protest, with no direct leverage over treaty enforcement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society, excluded,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global effort to prevent the spread of nuclear weapons by establishing a verification regime and a framework for peaceful nuclear energy cooperation, under the implicit understanding that nuclear weapon states retain their arsenals.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons exclusively to a few states, while transferring the obligation of non-acquisition and verification to all other states. It also transfers the burden of global nuclear security from disarmament to nonproliferation enforcement.
% ABSENT_VOICES: The global majority of non-nuclear weapon states, particularly those who feel their security is undermined by the nuclear monopoly, and global civil society advocating for disarmament. They would argue for a more balanced interpretation emphasizing Article VI's disarmament mandate.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the entire NPT regime would collapse, leading to a rapid proliferation of nuclear weapons as non-nuclear weapon states would no longer feel bound by their commitments without the perceived security guarantee or the hope of disarmament. The global security architecture would fundamentally reorganize.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent an uncontrolled spread of nuclear weapons beyond the initial five nuclear powers, while implicitly acknowledging the existing nuclear arsenals.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and their allies consistently attest that horizontal proliferation remains a live and severe threat, justifying the continued emphasis on Article IV and III. This is corroborated by intelligence assessments and international security analyses, though non-nuclear weapon states and disarmament advocates contest the framing that prioritizes nonproliferation over disarmament.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The high extractiveness (0.85) reflects the permanent and asymmetric burden placed on non-nuclear weapon states, who forgo nuclear weapons without a reciprocal commitment to disarmament from weapon states. Suppression (0.92) is severe due to the coercive power of weapon states (sanctions, military threats) to prevent proliferation, and the lack of viable exit options for non-nuclear weapon states. The high theater ratio (0.65) indicates that the performative commitment to Article VI disarmament serves as a cover for the actual function of maintaining the nuclear monopoly, with little genuine progress on disarmament. Accessibility collapse is high (0.78) because the international system offers few legitimate pathways for non-nuclear weapon states to acquire nuclear weapons or to credibly challenge the NPT's two-tier structure. Resistance (0.70) is substantial, primarily from non-nuclear weapon states and civil society, but it has not fundamentally altered the constraint's operation.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states perceive this as a necessary and legitimate framework for global stability, a 'rope' that prevents chaos. Non-nuclear weapon states, particularly those without security guarantees, perceive it as an extractive 'snare' that perpetuates an unjust power imbalance. The engine's classification as a Snare from the perspective of non-nuclear weapon states captures this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are clear beneficiaries and agenda-setters, shaping the interpretation and enforcement of the treaty to their advantage (low d). Non-nuclear weapon states are the primary payers/victims, bearing the costs of restraint and verification without the promised disarmament (high d). The IAEA, while technically neutral, acts as an enforcer of this reading, its mandate defined by the weapon states. Global civil society is excluded, their calls for disarmament largely ignored in the formal treaty mechanisms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_ambiguity,
    'Is Article VI of the NPT truly non-justiciable, or could a legal framework be developed to enforce its disarmament obligations?',
    'A ruling by the International Court of Justice or the establishment of a new treaty with binding disarmament timelines and enforcement mechanisms.',
    'If Article VI were deemed justiciable, the extractiveness of this reading would decrease, and the theater ratio would drop significantly, potentially reclassifying it from a Snare to a Tangled Rope or even a Scaffold if a genuine disarmament process began.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_ambiguity, conceptual, 'Ambiguity regarding the legal enforceability of NPT Article VI disarmament obligations.').

omega_variable(
    security_interest_vs_global_risk,
    'Does the security interest of nuclear weapon states in preventing horizontal proliferation genuinely outweigh the global catastrophic risk posed by the continued existence of their own arsenals?',
    'Comprehensive, independent risk assessments comparing the likelihood and impact of horizontal proliferation versus accidental or intentional use of existing arsenals, coupled with a shift in international norms.',
    'If the global risk of existing arsenals were deemed to outweigh the proliferation risk, the legitimacy of the nonproliferation primary reading would collapse, leading to increased pressure for disarmament and a reclassification of the constraint as a Snare with a much higher resistance metric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_interest_vs_global_risk, preference, 'The normative weighting of nuclear weapon states'' security interests against global catastrophic risk.').

omega_variable(
    two_tier_order_naturalness,
    'Is the two-tier nuclear order a ''natural'' and inevitable feature of international relations, or a constructed constraint maintained by the power of nuclear weapon states?',
    'Historical analysis of alternative security architectures, and observation of how the international system responds to shifts in power dynamics or the emergence of new non-nuclear security paradigms.',
    'If the two-tier order is revealed as a constructed constraint, the ''emerges_naturally'' claim (if present in a different reading) would be falsified, and the extractiveness and suppression of this reading would be seen as purely coercive, reinforcing its Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_tier_order_naturalness, empirical, 'Whether the nuclear two-tier order is a natural or constructed feature of international relations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('nonproliferation_primary') of the NPT Article IV/VI pairing kernel. It emphasizes nonproliferation over disarmament, in contrast to the 'grand_bargain' and 'abolitionist' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
