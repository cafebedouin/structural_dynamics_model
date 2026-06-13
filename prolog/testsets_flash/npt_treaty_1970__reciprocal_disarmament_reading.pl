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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI as Reciprocal Disarmament Obligation
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint represents the reading of the Nuclear Non-Proliferation
 *   Treaty (NPT) that emphasizes Article VI as a binding legal obligation for
 *   Nuclear Weapon States (NWS) to pursue disarmament with temporal urgency.
 *   It frames horizontal nonproliferation (NNWS not acquiring weapons) and
 *   vertical nonproliferation (NWS disarming) as a reciprocal bargain. From
 *   this perspective, the NPT is a Tangled Rope: it coordinates global
 *   security by preventing proliferation, but extracts from NWS by
 *   constraining their strategic autonomy and from NNWS by denying them a
 *   perceived security equalizer, while the lack of NWS disarmament
 *   verification creates an asymmetric burden.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary target/payer (institutional/constrained) — bears extraction from disarmament obligation, but also benefits from nonproliferation.
 *   - non_nuclear_weapon_states_coalition: Primary beneficiary (organized/constrained) — benefits from nonproliferation, but pays by forgoing nuclear weapons.
 *   - international_atomic_energy_agency: Agenda setter (institutional/constrained) — enforces horizontal nonproliferation, but lacks mandate for vertical disarmament verification.
 *   - global_security_advocates: Beneficiary (organized/analytical) — benefits from reduced proliferation risk, advocates for full Article VI implementation.
 *   - nws_military_industrial_complexes: Victim (institutional/trapped) — bears extraction from potential disarmament, resists constraints on modernization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.65).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.7).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI as Reciprocal Disarmament Obligation").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '16e81125-18b0-4210-9f0a-194e466adaab').
narrative_ontology:cs_kernel_codification('16e81125-18b0-4210-9f0a-194e466adaab', fixed_text).
narrative_ontology:cs_authority_grounding('16e81125-18b0-4210-9f0a-194e466adaab', lineage).
narrative_ontology:cs_interpretation_layer_present('16e81125-18b0-4210-9f0a-194e466adaab').
narrative_ontology:cs_reading_relation('16e81125-18b0-4210-9f0a-194e466adaab', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('16e81125-18b0-4210-9f0a-194e466adaab', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('16e81125-18b0-4210-9f0a-194e466adaab', foundational, article_vi_binding_and_urgent).
narrative_ontology:cs_axiom_status(article_vi_binding_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('16e81125-18b0-4210-9f0a-194e466adaab', article_vi_binding_and_urgent, deontological).
narrative_ontology:cs_axiom('16e81125-18b0-4210-9f0a-194e466adaab', foundational, horizontal_and_vertical_nonproliferation_reciprocal).
narrative_ontology:cs_axiom_status(horizontal_and_vertical_nonproliferation_reciprocal, holdable).
narrative_ontology:cs_axiom_grounding('16e81125-18b0-4210-9f0a-194e466adaab', horizontal_and_vertical_nonproliferation_reciprocal, conventional).
narrative_ontology:cs_reference_frame('16e81125-18b0-4210-9f0a-194e466adaab', original_npt_bargain_integrity).
narrative_ontology:cs_drift_state('16e81125-18b0-4210-9f0a-194e466adaab', contemporary_disarmament_stalemate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16e81125-18b0-4210-9f0a-194e466adaab', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, global_security_advocates).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_military_industrial_complexes).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because NWS are compelled to forgo strategic advantages and NNWS forgo a perceived security equalizer. Suppression (0.70) is significant due to the active enforcement of horizontal nonproliferation and the diplomatic pressure on NWS. Theater ratio (0.40) reflects the performative aspects of NWS disarmament commitments that often lack concrete action or verification. The rising trend in extractiveness and suppression over time reflects the increasing frustration of NNWS with the slow pace of NWS disarmament and the hardening of nonproliferation enforcement.
 *
 * PERSPECTIVAL GAP:
 *   NWS experience this constraint as a necessary, albeit sometimes inconvenient, framework for global stability that legitimizes their existing arsenals while preventing new entrants. NNWS, particularly those in the 'coalition,' experience it as an increasingly unfair bargain where their commitments are strictly enforced, but NWS obligations are not met with similar urgency or verification. The engine's per-seat classification should reflect this divergence, with NWS showing a more beneficiary-like profile (despite being 'victims' of the disarmament obligation) due to the lack of enforcement, and NNWS showing a more target-like profile.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'non_nuclear_weapon_states_coalition' are beneficiaries of horizontal nonproliferation (d=0.1), but also bear the cost of forgoing nuclear weapons (d=0.6), leading to a net moderate target directionality. 'Nuclear_weapon_states' are beneficiaries of horizontal nonproliferation (d=0.1) but are also the primary targets of the Article VI disarmament obligation (d=0.8), leading to a net moderate target directionality. The 'international_atomic_energy_agency' is an agenda setter with a symmetric directionality (d=0.5) as it enforces the regime but also faces limitations. 'NWS_military_industrial_complexes' are clear victims (d=0.9) as their existence is threatened by disarmament.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the NPT as a pure Snare by acknowledging the genuine coordination function of horizontal nonproliferation. However, it highlights the risk of mandatrophy in Article VI if the disarmament obligation remains unfulfilled, leading to a 'zombie' bargain where the original mandate (reciprocal disarmament) has atrophied but the constraint persists as an extractive mechanism for NNWS. The rising extractiveness and theater ratio in measurements indicate this drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforceability,
    'Is Article VI''s disarmament obligation genuinely enforceable, or is it aspirational without a verification mechanism?',
    'Establishment of a robust, intrusive, and universally accepted verification regime for NWS disarmament, or a UN Security Council resolution explicitly defining non-compliance and penalties.',
    'If enforceable, the constraint''s extractiveness from NWS would be higher and its suppression of NNWS alternatives lower; if purely aspirational, it functions more as a Snare for NNWS, with the NWS as beneficiaries of a legitimizing cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_enforceability, empirical, 'Ambiguity of Article VI''s binding force without verification.').

omega_variable(
    reciprocal_bargain_vs_oligopoly,
    'Is the NPT fundamentally a reciprocal bargain (disarmament for nonproliferation), or an oligopoly enforcement mechanism (NWS retain weapons, NNWS forgo them)?',
    'A shift in NWS behavior towards concrete, time-bound disarmament steps, or a formal declaration by a majority of NNWS that the bargain has failed.',
    'If a genuine reciprocal bargain, the constraint is a Tangled Rope with NWS as victims of their own commitments; if an oligopoly, it is a Snare for NNWS, with NWS as beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocal_bargain_vs_oligopoly, conceptual, 'Contested framing of the NPT''s core purpose.').

omega_variable(
    npt_kernel_reading_identification,
    'This constraint is the ''reciprocal_disarmament_reading'' of the ''npt_treaty_1970'' kernel. How would the classification change under the ''oligopoly_enforcement_reading'' or ''withdrawal_sovereignty_reading''?',
    'Analyzing the structural properties of the sibling readings as separate constraints.',
    'The ''oligopoly_enforcement_reading'' would likely classify as a Snare for NNWS, with higher extractiveness and suppression. The ''withdrawal_sovereignty_reading'' would emphasize the fragility of the entire regime, potentially classifying as a Piton or a highly contested Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_identification, conceptual, 'This constraint is one reading of the NPT kernel; other readings yield different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(npt__be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(npt__be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(npt__be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(npt__be_t50, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(npt__su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(npt__su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(npt__su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(npt__su_t50, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iran_nuclear_deal).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, north_korea_nuclear_program).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the NPT treaty kernel. The 'oligopoly_enforcement_reading' emphasizes Articles I-II and views Article VI as aspirational, while the 'withdrawal_sovereignty_reading' focuses on Article X. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
