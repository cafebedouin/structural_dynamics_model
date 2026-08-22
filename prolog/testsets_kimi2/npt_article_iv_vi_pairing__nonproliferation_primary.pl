% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Nonproliferation-Primary Reading: Article IV Conditional, Article VI Aspirational
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the nonproliferation_primary reading of the
 *   NPT Article IV-VI pairing kernel. Under this reading, Article IV peaceful
 *   technology access is strictly conditional on Article III verification
 *   compliance, while Article VI disarmament is aspirational and
 *   non-justiciable. Authority derives from weapon states' security interest
 *   in preventing horizontal proliferation, producing a permanently
 *   stabilized two-tier order. Weapon states retain arsenals without
 *   enforcement timelines; non-weapon states become perpetual
 *   restraint-bearers. The reading competes with the grand_bargain reading
 *   (reciprocal obligations) and the abolitionist reading (Article VI as
 *   mandatory disarmament). This constraint story authors the structural
 *   profile of the nonproliferation_primary interpretation as it operates in
 *   practice.
 *
 * KEY AGENTS:
 *   - weapon_states (agenda_setter/beneficiary): Set interpretive frame, retain arsenals, exempt from disarmament enforcement â institutional power, mobile exit.
 *   - non_weapon_states (payer): Bear restraint and verification burdens, receive conditional technology access â organized power, constrained exit.
 *   - iaea_verification_regime (agenda_setter): Administers Article III conditionality, gates Article IV â institutional power, mandate-constrained exit.
 *   - npt_expert_community (observer): Epistemic support for nonproliferation-primary frame â organized power, analytical exit.
 *   - abolitionist_advocates (excluded): Assert binding disarmament obligation, structurally marginalized â moderate power, constrained exit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.78).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Nonproliferation-Primary Reading: Article IV Conditional, Article VI Aspirational").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, '58b5db51-1dad-4b01-97ec-a8a09509fe07').
narrative_ontology:cs_kernel_codification('58b5db51-1dad-4b01-97ec-a8a09509fe07', fixed_text).
narrative_ontology:cs_authority_grounding('58b5db51-1dad-4b01-97ec-a8a09509fe07', extraction).
narrative_ontology:cs_interpretation_layer_present('58b5db51-1dad-4b01-97ec-a8a09509fe07').
narrative_ontology:cs_reading_relation('58b5db51-1dad-4b01-97ec-a8a09509fe07', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_reading_relation('58b5db51-1dad-4b01-97ec-a8a09509fe07', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('58b5db51-1dad-4b01-97ec-a8a09509fe07', foundational, article_vi_aspirational_status).
narrative_ontology:cs_axiom_status(article_vi_aspirational_status, holdable).
narrative_ontology:cs_axiom_grounding('58b5db51-1dad-4b01-97ec-a8a09509fe07', article_vi_aspirational_status, conventional).
narrative_ontology:cs_axiom('58b5db51-1dad-4b01-97ec-a8a09509fe07', foundational, horizontal_nonproliferation_imperative).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_imperative, holdable).
narrative_ontology:cs_axiom_grounding('58b5db51-1dad-4b01-97ec-a8a09509fe07', horizontal_nonproliferation_imperative, instrumental).
narrative_ontology:cs_reference_frame('58b5db51-1dad-4b01-97ec-a8a09509fe07', nuclear_oligopoly_stability).
narrative_ontology:cs_drift_state('58b5db51-1dad-4b01-97ec-a8a09509fe07', post_tpnw_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('58b5db51-1dad-4b01-97ec-a8a09509fe07', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, npt_nonproliferation_supremacy_doctrine).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__nonproliferation_primary, vertical_deterrence_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control NPT interpretive authority; treat Article VI as aspirational and non-justiciable; retain nuclear arsenals indefinitely; benefit from horizontal nonproliferation norms that freeze the status quo; modernize arsenals while non-weapon states accept permanent restraint.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states, beneficiary).

% Renounce nuclear weapons under Article II; accept IAEA safeguards under Article III as mandatory condition for Article IV peaceful technology access; bear verification burdens and technology denial risks; lack enforceable mechanism to compel weapon state disarmament under Article VI.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states, payer,
    organized, generational, constrained, global).

% Administer comprehensive safeguards and Additional Protocols under Article III; verify non-diversion of declared nuclear material; determine compliance that gates Article IV technology transfers; structurally authorized only to verify non-weapon state compliance, not weapon state disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_verification_regime, agenda_setter,
    institutional, generational, constrained, global).

% Provide technical and legal analysis affirming the Article III-IV linkage and the political (non-justiciable) character of Article VI; sustain the epistemic framework that treats weapon state security interests as the treaty's gravitational center.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, npt_expert_community, observer,
    organized, biographical, analytical, global).

% Assert Article VI creates binding disarmament obligations; advocate for nuclear prohibition through TPNW and humanitarian initiative; structurally marginalized in NPT review conferences where security discourse dominates; denied standing in weapon state security frameworks.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__nonproliferation_primary, abolitionist_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__nonproliferation_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevent horizontal nuclear proliferation by creating a verification-conditional technology access regime and establishing a legal norm against non-weapon state acquisition.
% TRANSFER_FUNCTION: Moves obligation and restraint from weapon states (who retain arsenals) to non-weapon states (who forgo acquisition and accept verification), while moving legitimacy and security guarantees to the weapon states' retention of deterrent capability.
% ABSENT_VOICES: Abolitionist advocates and non-aligned states arguing for enforceable Article VI timelines are present in Review Conferences but structurally excluded from interpretive authority; the TPNW constituency is excluded from the NPT's security-framework assumptions.
% DISAPPEARANCE_RATIONALE: Without the nonproliferation-primary interpretation, the verification-technology bargain loses coherence; non-weapon states would face weaker incentives to forgo weapons; the IAEA safeguards architecture loses its central legal anchor and horizontal proliferation would accelerate.
% FOUNDING_PROBLEM: Prevent rapid horizontal nuclear proliferation in the 1960s as more states approached weapons capability, creating an unstable multi-polar nuclear landscape.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states and the IAEA attest the problem is live. The non-aligned movement and international court advisory opinions (ICJ 1996) attest that the founding problem included disarmament as an integral obligation, not merely nonproliferation; the TPNW corroborates that the weapon-state security framing is contested from outside the beneficiary set.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__nonproliferation_primary, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__nonproliferation_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint permanently allocates disarmament obligation away from weapon states while imposing intrusive verification and technology conditionality on non-weapon states; the cost is borne by one class and the exemption by another. Suppression (0.78) is higher still because the regime actively suppresses horizontal proliferation through IAEA enforcement, export control regimes, and stigmatization of withdrawers, while also suppressing vertical disarmament pressure by rendering Article VI non-justiciable. Theater ratio (0.45) reflects significant performative maintenance: Review Conferences produce rhetorical reaffirmations of Article VI while weapon states modernize arsenals. Accessibility collapse (0.75) is high because once embedded in the NPT, non-weapon states face extreme costs of proliferation or withdrawal, and the Article VI alternative (disarmament) is interpreted out of reach. Resistance (0.55) captures the TPNW challenge, non-aligned critique, and cases like Iran and DPRK, but these remain structurally contained.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon state seat, the constraint is necessary security coordination preventing unstable nuclear multipolarity; from the non-weapon state seat, it is an asymmetric extraction of permanent restraint without reciprocal disarmament. The IAEA seat experiences it as a technical verification mandate. The engine computes these divergences from structural data: same treaty text, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon_states are structural beneficiaries (d near 0.0): the constraint subsidizes their security strategy by suppressing rival proliferation while exempting their arsenals from enforcement. Non_weapon_states are structural targets (d near 1.0): they pay through forgone weapons programs, acceptance of intrusive safeguards, and technology access conditionality. The IAEA regime sits between, administering extraction without capturing it. Abolitionist advocates are excluded from the directionality calculation entirely (excluded role).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because the founding problem (preventing rapid horizontal proliferation in the 1960s) remains live â nuclear acquisition by additional states would indeed reshape global security. However, under this reading the disarmament half of the mandate has been abandoned. The coordination function (nonproliferation) is genuine and ongoing, preventing classification as pure snare, while the asymmetric extraction (permanent weapon state exemption) prevents classification as rope. The persistence of real nonproliferation value alongside the atrophied disarmament commitment produces the tangled rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability_ambiguity,
    'Is Article VI a legally enforceable obligation or an aspirational political commitment under international law?',
    'ICJ contentious case or advisory opinion explicitly addressing the justiciability of Article VI timelines; or NPT review conference consensus language mandating specific disarmament benchmarks.',
    'If justiciable, the constraint shifts from tangled rope toward scaffold (enforced transition) or rope (reciprocal obligation); if aspirational, the two-tier hierarchy is structurally entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability_ambiguity, conceptual, 'Legal character of Article VI disarmament obligation').

omega_variable(
    npt_two_tier_naturalness,
    'Is the NPT''s two-tier membership structure a necessary coordination feature of nuclear governance, or a constructed hierarchy serving weapon state interests?',
    'Comparative analysis of alternative nuclear governance architectures (e.g., zone treaties, prohibition approaches) for stability outcomes; historical archival analysis of NPT negotiation records to determine intent.',
    'If necessary for coordination, extraction is the price of stability; if constructed hierarchy, the constraint approaches snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_two_tier_naturalness, conceptual, 'Whether the two-tier order is natural or constructed').

omega_variable(
    safeguards_cost_benefit_asymmetry,
    'Does the cost of IAEA safeguards and technology access denial borne by non-weapon states exceed the security benefit of prevented horizontal proliferation?',
    'Independent economic and security modeling of counterfactual proliferation scenarios versus actual safeguards burdens and technology forgone.',
    'If costs exceed benefits significantly, effective extraction is higher than the base metric suggests; if benefits dominate, the coordination function is stronger than the asymmetric structure implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safeguards_cost_benefit_asymmetry, empirical, 'Empirical balance of nonproliferation coordination benefit versus restraint cost').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 10, 0.25).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 30, 0.36).
narrative_ontology:measurement(npt__tr_t40, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 40, 0.4).
narrative_ontology:measurement(npt__tr_t55, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 55, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(npt__be_t40, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(npt__be_t55, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 55, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(npt__su_t40, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(npt__su_t55, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 55, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is the nonproliferation_primary reading of the NPT Article IV-VI pairing kernel. The kernel decomposes into three structurally distinct constraints because the natural-language label 'NPT regime' conflates competing interpretive claims with different epsilon values and stakeholder structures. This reading prioritizes weapon state security and nonproliferation enforcement; sibling readings treat the regime as reciprocal grand bargain or abolitionist prohibition framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
