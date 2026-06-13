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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   This constraint represents the interpretation of the Nuclear
 *   Non-Proliferation Treaty (NPT) where Article IV (peaceful nuclear energy)
 *   is strictly conditional on Article III (safeguards verification), and
 *   Article VI (disarmament) is considered aspirational and non-justiciable.
 *   The authority for this interpretation derives from the security interests
 *   of nuclear weapon states in preventing horizontal proliferation. This
 *   reading effectively stabilizes a two-tier nuclear order, with weapon
 *   states retaining their arsenals and non-weapon states bearing perpetual
 *   restraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.7).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.85).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing (Nonproliferation Primary Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '9f27d5ac-621a-4b29-9d0f-66783ad183ed').
narrative_ontology:cs_kernel_codification('9f27d5ac-621a-4b29-9d0f-66783ad183ed', fixed_text).
narrative_ontology:cs_authority_grounding('9f27d5ac-621a-4b29-9d0f-66783ad183ed', extraction).
narrative_ontology:cs_interpretation_layer_present('9f27d5ac-621a-4b29-9d0f-66783ad183ed').
narrative_ontology:cs_reading_relation('9f27d5ac-621a-4b29-9d0f-66783ad183ed', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_reading_relation('9f27d5ac-621a-4b29-9d0f-66783ad183ed', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('9f27d5ac-621a-4b29-9d0f-66783ad183ed', foundational, horizontal_proliferation_is_primary_threat).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_primary_threat, holdable).
narrative_ontology:cs_axiom_grounding('9f27d5ac-621a-4b29-9d0f-66783ad183ed', horizontal_proliferation_is_primary_threat, empirically_contingent).
narrative_ontology:cs_axiom('9f27d5ac-621a-4b29-9d0f-66783ad183ed', foundational, nuclear_deterrence_is_legitimate).
narrative_ontology:cs_axiom_status(nuclear_deterrence_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('9f27d5ac-621a-4b29-9d0f-66783ad183ed', nuclear_deterrence_is_legitimate, conventional).
narrative_ontology:cs_reference_frame('9f27d5ac-621a-4b29-9d0f-66783ad183ed', weapon_state_security_paradigm).
narrative_ontology:cs_drift_state('9f27d5ac-621a-4b29-9d0f-66783ad183ed', contemporary_treaty_review_cycle, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9f27d5ac-621a-4b29-9d0f-66783ad183ed', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, iaea).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, global_civil_society).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it offers a genuine coordination function (preventing proliferation) but couples it with asymmetric extraction. Non-nuclear-weapon states coordinate by foregoing nuclear weapons development, but pay by accepting a permanent security disadvantage and the non-enforceability of disarmament obligations. Extraction is high (0.7) due to the indefinite deferral of disarmament and the perpetuation of a two-tiered system. Suppression is very high (0.85) as the international system actively enforces nonproliferation through sanctions, military threats, and diplomatic pressure, while resisting efforts to enforce disarmament. Theater ratio is moderate (0.4) as disarmament rhetoric continues, but the actual practice of weapon states shows little movement towards the stated goal.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states experience this as a Rope, as it secures their nonproliferation goals while preserving their arsenals. Non-nuclear-weapon states, particularly those with advanced nuclear energy programs, experience it as a Snare, as it imposes significant costs and limits their sovereign choices without reciprocal disarmament. The IAEA, as an institutional actor, experiences it as a Tangled Rope, balancing its verification mandate with the political realities of weapon state influence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are primary beneficiaries (d=0.0-0.1) as their security interests are prioritized and their arsenals are protected from enforcement. The IAEA is a beneficiary (d=0.1-0.2) as its mandate and funding are tied to the nonproliferation regime. Non-nuclear-weapon states are victims (d=0.8-0.9) as they bear the costs of restraint without the promised disarmament. Global civil society is also a victim (d=0.7-0.8) as its calls for disarmament are largely ignored.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure Rope by highlighting the asymmetric extraction inherent in the nonproliferation-first interpretation. It also prevents mislabeling it as a pure Snare by acknowledging the genuine, albeit unevenly distributed, coordination function of preventing horizontal proliferation. The 'mandate' of nonproliferation is live, but the 'mandate' of disarmament is effectively atrophied under this reading, leading to the Tangled Rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_ambiguity,
    'Is the NPT a grand bargain of reciprocal obligations, or primarily a nonproliferation instrument?',
    'Analysis of state practice, treaty review conference outcomes, and legal interpretations by international courts or advisory bodies over time.',
    'If read as a grand bargain, the legitimacy of Article IV for non-nuclear-weapon states is conditional on Article VI progress, potentially reclassifying the constraint as a Snare if disarmament obligations are systematically ignored. If read as nonproliferation primary, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_ambiguity, conceptual, 'This constraint is one reading of the NPT kernel, emphasizing nonproliferation over disarmament.').

omega_variable(
    article_vi_justiciability,
    'Is Article VI of the NPT a legally binding and justiciable obligation, or merely an aspirational statement of intent?',
    'Referral to the International Court of Justice for an advisory opinion or a binding ruling in a contentious case, or a clear consensus among NPT state parties.',
    'If found justiciable, the current reading''s claim of non-justiciability would be overridden, increasing the extractiveness and suppression for non-nuclear-weapon states and potentially shifting the constraint towards a Snare due to unfulfilled obligations. If found aspirational, the current classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Ambiguity regarding the legal enforceability of nuclear disarmament obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 10, 0.35).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 20, 0.38).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 20, 0.83).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing kernel. The other readings are 'npt_article_iv_vi_pairing__grand_bargain' and 'npt_article_iv_vi_pairing__abolitionist'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
