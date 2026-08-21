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
 *   NPT's Article IV/VI pairing. In this reading, Article IV (peaceful uses
 *   of nuclear energy) is strictly conditional on Article III verification,
 *   while Article VI (disarmament) is interpreted as an aspirational,
 *   non-justiciable goal. Authority for the regime derives from the security
 *   interests of nuclear-weapon states in preventing horizontal
 *   proliferation. This interpretation stabilizes a two-tier order where
 *   weapon states maintain their arsenals and non-weapon states bear
 *   perpetual restraint, with Article VI's disarmament timeline effectively
 *   unenforceable.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.78).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing (Nonproliferation Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, 'e6452b2f-3401-4677-ae75-2e35c755650d').
narrative_ontology:cs_kernel_codification('e6452b2f-3401-4677-ae75-2e35c755650d', fixed_text).
narrative_ontology:cs_authority_grounding('e6452b2f-3401-4677-ae75-2e35c755650d', extraction).
narrative_ontology:cs_interpretation_layer_present('e6452b2f-3401-4677-ae75-2e35c755650d').
narrative_ontology:cs_reading_relation('e6452b2f-3401-4677-ae75-2e35c755650d', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('e6452b2f-3401-4677-ae75-2e35c755650d', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('e6452b2f-3401-4677-ae75-2e35c755650d', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('e6452b2f-3401-4677-ae75-2e35c755650d', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('e6452b2f-3401-4677-ae75-2e35c755650d', foundational, weapon_state_security_interests_are_paramount).
narrative_ontology:cs_axiom_status(weapon_state_security_interests_are_paramount, holdable).
narrative_ontology:cs_axiom_grounding('e6452b2f-3401-4677-ae75-2e35c755650d', weapon_state_security_interests_are_paramount, conventional).
narrative_ontology:cs_axiom('e6452b2f-3401-4677-ae75-2e35c755650d', secondary, article_vi_is_aspirational_not_binding).
narrative_ontology:cs_axiom_status(article_vi_is_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('e6452b2f-3401-4677-ae75-2e35c755650d', article_vi_is_aspirational_not_binding, conventional).
narrative_ontology:cs_reference_frame('e6452b2f-3401-4677-ae75-2e35c755650d', horizontal_nonproliferation_priority).
narrative_ontology:cs_drift_state('e6452b2f-3401-4677-ae75-2e35c755650d', contemporary_proliferation_challenges, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6452b2f-3401-4677-ae75-2e35c755650d', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five nuclear-weapon states (P5) under the NPT. They define the terms of non-proliferation, enforce Article III verification on non-weapon states, and interpret Article VI as an aspirational goal without binding disarmament timelines. Their security interests are paramount, justifying their continued arsenals and the two-tier order.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% States that have foresworn nuclear weapons under the NPT. They bear the burden of strict non-proliferation commitments and IAEA verification (Article III), while receiving limited benefits from civilian nuclear cooperation (Article IV) and seeing no concrete progress on weapon state disarmament (Article VI). Their security is conditional on weapon state assurances.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, payer,
    organized, biographical, constrained, global).

% The International Atomic Energy Agency, responsible for verifying non-proliferation commitments under Article III. Its mandate and legitimacy are derived from the NPT regime, making it a beneficiary of the existing structure, even as it faces challenges in enforcement.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea, beneficiary,
    institutional, biographical, constrained, global).

% Advocates for nuclear disarmament and a more equitable non-proliferation regime. Their calls for weapon state compliance with Article VI are largely marginalized by the dominant interpretation, and they have no direct leverage over treaty enforcement or interpretation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society, excluded,
    powerless, generational, trapped, global).

% Academics, think tanks, and independent experts who analyze the NPT regime, its effectiveness, and its challenges. They often highlight the structural inequalities and the gap between the treaty's stated goals and its practical implementation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the horizontal proliferation of nuclear weapons by establishing a global norm and verification regime, while theoretically allowing for peaceful uses of nuclear energy.
% TRANSFER_FUNCTION: Transfers the burden of nuclear restraint and verification costs from nuclear-weapon states to non-nuclear-weapon states, in exchange for conditional access to civilian nuclear technology and security assurances. It also transfers the responsibility for disarmament from weapon states to an aspirational, non-justiciable future.
% ABSENT_VOICES: States that have withdrawn from the NPT (e.g., North Korea), states that never joined (e.g., India, Pakistan, Israel), and global civil society advocating for immediate disarmament. They would argue against the legitimacy of the two-tier system and the weapon states' interpretation of Article VI.
% DISAPPEARANCE_RATIONALE: The NPT is the foundational treaty of the global non-proliferation regime. Its disappearance would likely lead to a rapid increase in horizontal proliferation, collapse of international nuclear cooperation, and a fundamental destabilization of global security.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent more states from acquiring nuclear weapons, while allowing peaceful uses of nuclear energy under international safeguards.
% FOUNDING_PROBLEM_CORROBORATION: International security reports, UN resolutions, and the ongoing efforts to prevent proliferation (e.g., Iran, North Korea) corroborate that the core problem of nuclear proliferation remains live. However, the *balance* of the original bargain is contested by non-weapon states and civil society.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.85) because non-weapon states bear significant costs (foregoing nuclear weapons, accepting intrusive verification) with minimal reciprocal benefit (no disarmament from P5). Suppression is high (0.78) due to the coercive power of weapon states (sanctions, military threats) against non-compliant states, and the lack of viable exit options for non-weapon states seeking security. Theater ratio is moderate (0.45) as discussions around Article VI provide a performative cover for the lack of actual disarmament, while Article III enforcement is very real. Accessibility collapse is high (0.70) because the NPT regime is the dominant framework for nuclear legitimacy, making alternatives difficult to pursue without severe international repercussions. Resistance is moderate (0.60) as non-weapon states frequently voice discontent at NPT Review Conferences but largely remain within the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of weapon states, this reading presents a necessary and effective mechanism for global security (a Rope or even a Mountain of geopolitical reality). From the perspective of non-weapon states, it is a discriminatory system that extracts security and sovereignty in an asymmetric manner (a Snare). The engine's classification as Snare reflects the structural asymmetry and extraction, independent of the weapon states' self-justifying narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states are the primary beneficiaries and agenda-setters, as the regime secures their non-proliferation interests while exempting their arsenals. Non-weapon states are the primary victims, bearing the costs of restraint and verification without the promised disarmament. The IAEA benefits from its central role in the verification regime. Global civil society is excluded from meaningful influence on the regime's core interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of preventing horizontal proliferation remains live. However, the 'grand bargain' aspect, which implied reciprocal disarmament, has atrophied under this reading. The constraint persists not because the disarmament mandate is fulfilled, but because the weapon states' security interests in non-proliferation are actively enforced, effectively transforming a potential Rope (reciprocal bargain) into a Snare (asymmetric extraction under cover of a collective good).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_reading_ambiguity,
    'Is the ''nonproliferation primary'' interpretation of the NPT the only valid reading, or is it one of several competing, structurally distinct interpretations?',
    'Analysis of state practice, diplomatic statements, and legal scholarship from diverse actors over time to identify consistent alternative interpretations.',
    'If other readings (e.g., ''grand_bargain'', ''abolitionist'') are equally coherent and held by significant actors, then the NPT is a kernel generating multiple distinct constraints, each with its own classification. This would shift the analysis from a single ''Snare'' to a contested kernel with multiple, potentially different, classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_reading_ambiguity, conceptual, 'Ambiguity regarding the NPT''s core interpretive framework.').

omega_variable(
    article_vi_justiciability,
    'Is Article VI truly non-justiciable and merely aspirational, or is this interpretation a strategic choice by weapon states to avoid binding disarmament obligations?',
    'International legal rulings or a shift in weapon state policy to accept concrete, verifiable disarmament timelines and mechanisms.',
    'If Article VI were deemed justiciable, the extractiveness and suppression of the ''nonproliferation primary'' reading would increase significantly, as it would be seen as actively violating a binding obligation. The claimed type would be more firmly a Snare, with a clearer victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, empirical, 'The legal status and enforceability of NPT Article VI.').

omega_variable(
    proliferation_risk_attribution,
    'Is horizontal proliferation primarily driven by the ambition of non-weapon states, or by the security dilemmas created by existing weapon state arsenals and their lack of disarmament?',
    'Empirical studies correlating proliferation attempts with shifts in regional security balances and weapon state disarmament progress, rather than solely focusing on non-weapon state capabilities.',
    'If weapon state arsenals are a primary driver of proliferation, the ''nonproliferation primary'' reading''s justification for weapon state exemption would weaken, exposing its extractive nature more clearly. This would reinforce its Snare classification and highlight the underlying power asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_risk_attribution, empirical, 'The root causes of nuclear proliferation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt__tr_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.75).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(npt__be_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2015, 0.83).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(npt__su_t2015, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(npt__su_t2025, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iran_nuclear_deal).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, north_korea_nuclear_program).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the NPT Article IV/VI pairing kernel. This 'nonproliferation primary' reading emphasizes horizontal non-proliferation and weapon state security interests, interpreting Article VI as aspirational. It differs from the 'grand_bargain' reading (which stresses reciprocity) and the 'abolitionist' reading (which prioritizes disarmament and humanitarian law).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
