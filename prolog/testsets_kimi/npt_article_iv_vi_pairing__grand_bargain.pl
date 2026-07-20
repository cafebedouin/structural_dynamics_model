% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: NPT Grand Bargain: Reciprocal Article IV/VI Conditionality
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint models the NPT Article IV/VI pairing interpreted through
 *   the grand bargain reading: non-weapon state restraint is a reciprocal
 *   obligation conditioned on weapon state disarmament progress. The kernel
 *   is contestedâweapon states read Article VI as aspirational, while
 *   abolitionists reject the legitimacy of the entire framework. This JSON
 *   instantiates ONLY the grand_bargain reading as a clean, epsilon-invariant
 *   constraint with its own beneficiary/victim structure, structural data,
 *   and computed directionality.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary beneficiary and agenda-setter (institutional/constrained) â maintains arsenal monopoly while extracting non-proliferation compliance
 *   - non_nuclear_weapon_states: Primary payer (organized/constrained) â forgoes nuclear option while disarmament is delayed
 *   - international_atomic_energy_agency: Enforcement administrator (institutional/constrained) â verifies safeguards without adjudicating reciprocity
 *   - abolitionist_advocacy_networks: Excluded voice (moderate/mobile) â rejects legitimacy of the grand bargain frame entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.72).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.65).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Grand Bargain: Reciprocal Article IV/VI Conditionality").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '7a76005c-8572-4be5-ab43-b76c73fc28b3').
narrative_ontology:cs_kernel_codification('7a76005c-8572-4be5-ab43-b76c73fc28b3', fixed_text).
narrative_ontology:cs_authority_grounding('7a76005c-8572-4be5-ab43-b76c73fc28b3', lineage).
narrative_ontology:cs_interpretation_layer_present('7a76005c-8572-4be5-ab43-b76c73fc28b3').
narrative_ontology:cs_reading_relation('7a76005c-8572-4be5-ab43-b76c73fc28b3', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('7a76005c-8572-4be5-ab43-b76c73fc28b3', npt_article_iv_vi_pairing__abolitionist, influences).
narrative_ontology:cs_axiom('7a76005c-8572-4be5-ab43-b76c73fc28b3', foundational, article_vi_reciprocal_enforceable).
narrative_ontology:cs_axiom_status(article_vi_reciprocal_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('7a76005c-8572-4be5-ab43-b76c73fc28b3', article_vi_reciprocal_enforceable, conventional).
narrative_ontology:cs_axiom('7a76005c-8572-4be5-ab43-b76c73fc28b3', foundational, article_iv_legitimate_conditional).
narrative_ontology:cs_axiom_status(article_iv_legitimate_conditional, holdable).
narrative_ontology:cs_axiom_grounding('7a76005c-8572-4be5-ab43-b76c73fc28b3', article_iv_legitimate_conditional, conventional).
narrative_ontology:cs_reference_frame('7a76005c-8572-4be5-ab43-b76c73fc28b3', reciprocal_bargain_1968).
narrative_ontology:cs_drift_state('7a76005c-8572-4be5-ab43-b76c73fc28b3', contemporary_npt_review_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a76005c-8572-4be5-ab43-b76c73fc28b3', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain nuclear arsenals and extended deterrence guarantees while controlling the pace of disarmament negotiations. They benefit from a treaty framework that legally binds most other states to non-acquisition while preserving their own status. They set agendas at Review Conferences and in the Security Council. Exit via treaty withdrawal is legally possible but politically prohibitive and would undermine the security architecture they depend on.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states, agenda_setter).

% Refrain from developing nuclear weapons in exchange for promised disarmament and peaceful technology cooperation. They bear the cost of permanent strategic asymmetry and delayed disarmament timelines. They gain reduced regional proliferation risk. Most remain in the treaty because withdrawal triggers sanctions and diplomatic isolation, though they organize through groups like the Non-Aligned Movement to demand compliance.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, non_nuclear_weapon_states, beneficiary).

% Verifies non-proliferation commitments through safeguards agreements and inspections. It reports compliance findings to the international community but lacks authority to adjudicate Article VI disarmament progress or to enforce the reciprocal bargain. Its mandate is defined by the treaty text and member state directives.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Campaign for complete nuclear prohibition through humanitarian law frameworks and the Treaty on the Prohibition of Nuclear Weapons. They argue that any treaty permitting nuclear possession is illegitimate. They participate marginally in NPT review processes but their core claim is systematically excluded from the formal state consensus documents.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, abolitionist_advocacy_networks, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__grand_bargain, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__grand_bargain, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation by establishing a legally binding framework in which non-weapon states forgo nuclear weapons acquisition in exchange for weapon-state disarmament progress and access to peaceful nuclear technology.
% TRANSFER_FUNCTION: Moves nuclear forbearance and strategic restraint from non-weapon states to the international order; moves promises of disarmament and peaceful technology from weapon states to non-weapon states, though the latter remain largely unfulfilled.
% ABSENT_VOICES: Abolitionist advocates and Treaty on the Prohibition of Nuclear Weapons signatories who reject the legitimacy of any framework that permits nuclear possession; nuclear-hedging states that would expand their programs if the reciprocal condition were enforced; independent legal scholars arguing Article VI creates individual state obligations rather than merely collective promises.
% DISAPPEARANCE_RATIONALE: If the grand bargain conditionality vanished, non-weapon states would reassess their non-proliferation commitments, the NPT's political foundation for the majority of states would collapse, and the treaty would likely face mass withdrawal or demands for immediate binding disarmament verification.
% FOUNDING_PROBLEM: Uncontrolled horizontal nuclear proliferation in the 1960s threatened to create dozens of nuclear-armed states, raising catastrophic war risks and destabilizing the superpower balance.
% FOUNDING_PROBLEM_CORROBORATION: Declassified negotiating records and UN archives from 1965-1968 corroborate the grand bargain intent. However, weapon states now characterize Article VI as aspirational, while non-weapon states and independent legal scholars outside the weapon-state camp contest that reading, citing the original quid pro quo.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.72) because weapon states have retained arsenals for decades while non-weapon states remain legally bound to forbearance, with disarmament timelines continuously deferred. Suppression is moderate-high (0.65): enforcement via IAEA safeguards, Security Council sanctions, and diplomatic isolation keeps most non-weapon states in compliance despite the broken reciprocity, though resistance through NAM coordination and the TPNW is significant. Theater ratio (0.55) reflects the increasingly performative nature of NPT Review Conferences, where reaffirmations of the grand bargain rhetoric outpace actual disarmament. Accessibility collapse (0.60) captures that alternatives like the TPNW exist but lack the institutional weight of the NPT. Resistance (0.55) reflects sustained but institutionally marginalized demands from non-weapon states.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear-weapon-state seat, the arrangement is a legitimate security order that coordinates global non-proliferation and preserves strategic stability; the disarmament timeline is a complex process, not a breach. From the non-weapon-state seat, the same structure appears as an unfulfilled contract that locks them into permanent strategic inferiority while weapon states modernize arsenals. The engine computes this divergence from the structural dataâbeneficiary status, constrained exit, and victim declarationsâwithout requiring the author to reconcile the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are declared beneficiaries with constrained exit (withdrawing would destroy the security architecture they benefit from), placing their directionality near the full-beneficiary end. Non-nuclear weapon states are declared victims with constrained exit (withdrawal triggers sanctions and isolation), placing their directionality near the full-target end. The IAEA sits near symmetric: it administers the constraint but neither collects the primary benefit nor bears the primary cost. Abolitionist networks are excluded from the formal bargain; their mobile exit (they can advocate outside the NPT framework) places them closer to the beneficiary end than the trapped non-weapon states, though they do not materially benefit from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The NPT's founding coordination functionâpreventing widespread nuclear proliferationâremains partially live, which prevents classifying the constraint as a pure Snare. However, the reciprocal disarmament obligation has atrophied into political theater, extracting indefinite restraint from non-weapon states without delivering commensurate disarmament. This asymmetry prevents classifying it as a pure Rope. The Tangled Rope classification captures both the genuine coordination benefit (reduced horizontal proliferation) and the asymmetric extraction (vertical proliferation maintained under the cover of a broken bargain).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a legally enforceable reciprocal obligation or merely an aspirational political commitment?',
    'Binding ICJ advisory opinion or international tribunal ruling on the justiciability of Article VI and the permissibility of suspension of Article IV obligations in response to breach.',
    'If Article VI is enforceable, weapon states are in material breach and non-weapon states may suspend obligations; if aspirational, the grand bargain reading collapses toward the nonproliferation_primary reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'Legal character of Article VI as obligation vs aspiration').

omega_variable(
    nnws_collective_enforcement_capacity,
    'Can non-weapon states credibly coordinate collective withdrawal or treaty suspension, or does power asymmetry render the reciprocal condition structurally inert?',
    'Observed coalition behavior at NPT Review Conferences; actual instances of safeguards suspension or treaty withdrawal by non-weapon states in response to Article VI non-compliance.',
    'If collective enforcement is impossible, the reciprocal condition is theater and the constraint functions as extraction regardless of its legal form; if possible, the grand bargain retains structural teeth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_collective_enforcement_capacity, empirical, 'Collective action capacity of non-weapon states to enforce reciprocity').

omega_variable(
    withdrawal_cost_asymmetry,
    'Is the cost of NPT withdrawal structurally higher for non-weapon states than for weapon states, and does this asymmetry convert the bargain into extraction?',
    'Comparative case analysis of North Korea''s post-withdrawal trajectory versus hypothetical weapon-state withdrawal scenarios; sanctions and diplomatic isolation differentials.',
    'If withdrawal is prohibitively costly for non-weapon states but not for weapon states, the bargain lacks exit symmetry and functions as a one-sided lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_cost_asymmetry, empirical, 'Asymmetric exit costs between weapon and non-weapon states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 10, 0.25).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 20, 0.35).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 30, 0.45).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 40, 0.5).
narrative_ontology:measurement(npt__tr_t50, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(npt__be_t50, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(npt__su_t50, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT Article IV/VI pairing kernel, which decomposes into three structurally distinct constraints: the nonproliferation_primary reading (weapon-state security framing), the grand_bargain reading (reciprocal conditionality), and the abolitionist reading (humanitarian prohibition framing). Each reading has distinct beneficiaries, victim sets, and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
