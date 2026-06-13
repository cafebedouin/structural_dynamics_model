% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'abolitionist' reading of the NPT's
 *   Article IV (peaceful nuclear energy) and Article VI (disarmament
 *   obligation) pairing. In this reading, Article VI's mandate for complete
 *   disarmament is paramount, and Article IV's allowance for peaceful nuclear
 *   energy is illegitimate if it perpetuates dual-use proliferation risk. The
 *   authority for this interpretation derives from international humanitarian
 *   law and the precedent set by the Treaty on the Prohibition of Nuclear
 *   Weapons (TPNW). This reading views the NPT itself as insufficient and
 *   delegitimized by the continued possession of nuclear weapons, asserting
 *   that weapon possession is categorically illegal with no distinction
 *   between peaceful and military programs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.9).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '9211c248-349e-4096-894e-cac7e76208d0').
narrative_ontology:cs_kernel_codification('9211c248-349e-4096-894e-cac7e76208d0', fixed_text).
narrative_ontology:cs_authority_grounding('9211c248-349e-4096-894e-cac7e76208d0', lineage).
narrative_ontology:cs_interpretation_layer_present('9211c248-349e-4096-894e-cac7e76208d0').
narrative_ontology:cs_reading_relation('9211c248-349e-4096-894e-cac7e76208d0', npt_article_iv_vi_pairing__nonproliferation_primary, influences).
narrative_ontology:cs_reading_relation('9211c248-349e-4096-894e-cac7e76208d0', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('9211c248-349e-4096-894e-cac7e76208d0', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('9211c248-349e-4096-894e-cac7e76208d0', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('9211c248-349e-4096-894e-cac7e76208d0', foundational, article_iv_conditional_on_disarmament).
narrative_ontology:cs_axiom_status(article_iv_conditional_on_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('9211c248-349e-4096-894e-cac7e76208d0', article_iv_conditional_on_disarmament, conventional).
narrative_ontology:cs_reference_frame('9211c248-349e-4096-894e-cac7e76208d0', universal_prohibition_norm).
narrative_ontology:cs_drift_state('9211c248-349e-4096-894e-cac7e76208d0', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9211c248-349e-4096-894e-cac7e76208d0', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, global_civil_society).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, future_generations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the continued existence of nuclear weapons, despite Article VI, imposes an existential cost on non-nuclear states and global civil society. Suppression (0.9) is also high, reflecting the power asymmetry where nuclear weapon states actively suppress disarmament initiatives and maintain their nuclear status through deterrence doctrines and institutional inertia. The theater ratio (0.1) is low, as the abolitionist reading sees little genuine disarmament effort, viewing most 'disarmament' rhetoric as cover for maintaining arsenals. Resistance (0.8) is high, driven by the strong advocacy from non-nuclear states and civil society for complete disarmament.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapon states (agenda_setter) experience this constraint as a persistent, but manageable, diplomatic pressure that they can largely resist or reinterpret. Non-nuclear weapon states and global civil society (payers/victims) experience it as a profound structural injustice and an existential threat, where the NPT's promise of disarmament remains unfulfilled, perpetuating a snare-like dynamic.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are the primary beneficiaries (d=0.0-0.1) as they maintain their arsenals and status while non-nuclear states disarm. Non-nuclear weapon states, global civil society, and future generations are the victims/targets (d=0.8-1.0), bearing the costs of proliferation risk and unfulfilled disarmament promises. Humanitarian law advocates are beneficiaries (d=0.0-0.1) as their legal framework gains traction.
 *
 * MANDATROPHY ANALYSIS:
 *   This abolitionist reading highlights the mandatrophy of the NPT's disarmament promise. The original mandate to disarm has atrophied, replaced by a de facto legitimization of nuclear weapon states' arsenals. The classification as a snare from the perspective of non-nuclear states and civil society prevents mislabeling this as a coordination problem, instead exposing the asymmetric extraction and suppression inherent in the current nuclear order. The persistence of the constraint is due to the active enforcement by nuclear weapon states to maintain their status, rather than genuine coordination towards disarmament.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_legitimacy_source,
    'Does the NPT''s legitimacy primarily derive from its non-proliferation function (Article I/II/III) or its disarmament obligation (Article VI)?',
    'Analysis of state practice and declarations, particularly from non-nuclear weapon states, regarding their continued adherence to the NPT in the absence of disarmament progress.',
    'If legitimacy is primarily from non-proliferation, the abolitionist reading''s delegitimization of Article IV is weaker. If from disarmament, the abolitionist reading gains significant force, potentially leading to NPT withdrawals or reclassification of the NPT itself as a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_legitimacy_source, conceptual, 'The foundational source of the NPT''s legitimacy.').

omega_variable(
    dual_use_separability,
    'Is the ''peaceful'' use of nuclear energy (Article IV) genuinely separable from its ''military'' potential, or does it inherently perpetuate proliferation risk?',
    'Technological advancements in proliferation-resistant fuel cycles and independent verification of their efficacy, or a consensus among nuclear experts on the inherent dual-use nature of certain technologies.',
    'If inseparable, the abolitionist reading''s critique of Article IV is strengthened, reinforcing the view that any peaceful program contributes to proliferation risk. If separable, Article IV could be seen as a legitimate coordination mechanism, weakening the abolitionist claim of illegitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_separability, empirical, 'The inherent dual-use nature of nuclear technology.').

omega_variable(
    abolitionist_reading_status,
    'Is this abolitionist reading a fringe interpretation or a growing, mainstream challenge to the NPT regime?',
    'Tracking the number of states ratifying the TPNW, the voting patterns in UN General Assembly resolutions on nuclear disarmament, and the frequency and content of statements by non-nuclear weapon states at NPT Review Conferences.',
    'If fringe, the constraint''s effective suppression is higher, as the challenge is easily dismissed. If mainstream, the resistance metric is more impactful, and the pressure on nuclear weapon states to comply with Article VI increases, potentially shifting the NPT''s classification towards a tangled rope or even a scaffold if genuine disarmament negotiations begin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolitionist_reading_status, empirical, 'The political and legal traction of the abolitionist interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1968, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(npt__tr_t1992, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1992, 0.15).
narrative_ontology:measurement(npt__tr_t2004, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2004, 0.12).
narrative_ontology:measurement(npt__tr_t2016, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2016, 0.11).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(npt__be_t1992, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1992, 0.75).
narrative_ontology:measurement(npt__be_t2004, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2004, 0.8).
narrative_ontology:measurement(npt__be_t2016, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2016, 0.83).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1992, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1992, 0.8).
narrative_ontology:measurement(npt__su_t2004, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2004, 0.85).
narrative_ontology:measurement(npt__su_t2016, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2016, 0.88).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_prohibition_norm).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Article IV/VI pairing kernel. This 'abolitionist' reading emphasizes the primacy of Article VI's disarmament mandate and the illegitimacy of Article IV if it enables proliferation, drawing authority from humanitarian law and the TPNW. It contrasts with the 'nonproliferation_primary' reading (emphasizing horizontal non-proliferation) and the 'grand_bargain' reading (emphasizing reciprocal obligations).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
