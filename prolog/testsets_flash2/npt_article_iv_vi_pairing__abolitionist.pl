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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing (Abolitionist Reading)
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of the NPT's Article
 *   IV (peaceful uses) and Article VI (disarmament obligations). It asserts
 *   that Article VI mandates complete disarmament and that Article IV is
 *   illegitimate if it perpetuates dual-use proliferation risk. Authority for
 *   this reading derives from humanitarian law and the Treaty on the
 *   Prohibition of Nuclear Weapons (TPNW). This reading fundamentally
 *   challenges the legitimacy of nuclear weapon possession and any nuclear
 *   program with dual-use potential, aiming to delegitimize the NPT itself as
 *   insufficient.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.85).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.75).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.85).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, snare).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '3f76a677-6649-404f-8e55-f1af2f077a38').
narrative_ontology:cs_kernel_codification('3f76a677-6649-404f-8e55-f1af2f077a38', fixed_text).
narrative_ontology:cs_authority_grounding('3f76a677-6649-404f-8e55-f1af2f077a38', lineage).
narrative_ontology:cs_interpretation_layer_present('3f76a677-6649-404f-8e55-f1af2f077a38').
narrative_ontology:cs_reading_relation('3f76a677-6649-404f-8e55-f1af2f077a38', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('3f76a677-6649-404f-8e55-f1af2f077a38', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_axiom('3f76a677-6649-404f-8e55-f1af2f077a38', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('3f76a677-6649-404f-8e55-f1af2f077a38', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('3f76a677-6649-404f-8e55-f1af2f077a38', foundational, article_vi_mandates_immediate_disarmament).
narrative_ontology:cs_axiom_status(article_vi_mandates_immediate_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('3f76a677-6649-404f-8e55-f1af2f077a38', article_vi_mandates_immediate_disarmament, conventional).
narrative_ontology:cs_reference_frame('3f76a677-6649-404f-8e55-f1af2f077a38', humanitarian_law_supremacy).
narrative_ontology:cs_drift_state('3f76a677-6649-404f-8e55-f1af2f077a38', contemporary_nuclear_weapon_state_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3f76a677-6649-404f-8e55-f1af2f077a38', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, states_relying_on_nuclear_deterrence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, international_humanitarian_law_advocates).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_supremacy).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, nuclear_weapons_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states are the primary targets of the abolitionist reading, which demands their complete disarmament and delegitimizes their possession of nuclear weapons. Their security doctrines are deeply intertwined with nuclear deterrence, making exit from this position an identity-locked choice.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer,
    institutional, generational, identity_locked, global).

% States that actively advocate for complete nuclear disarmament, often signatories to the TPNW. They seek to reframe the NPT as a disarmament treaty first and foremost, challenging the legitimacy of nuclear weapon possession and the dual-use interpretation of Article IV.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states_abolitionist, agenda_setter,
    organized, generational, constrained, global).

% States that do not possess nuclear weapons but rely on extended deterrence from nuclear weapon states. The abolitionist reading challenges the legitimacy of this security posture, imposing a cost on their strategic alignment.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, states_relying_on_nuclear_deterrence, payer,
    powerful, biographical, identity_locked, global).

% Organizations and legal scholars who champion the supremacy of humanitarian law and the prohibition of weapons of mass destruction. This reading aligns with and strengthens their normative framework.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, international_humanitarian_law_advocates, beneficiary,
    organized, civilizational, analytical, universal).

% The International Atomic Energy Agency, tasked with verifying peaceful nuclear uses. This reading complicates its mandate by questioning the legitimacy of any dual-use nuclear program, even if declared 'peaceful' under Article IV.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, iaea, observer,
    institutional, generational, constrained, global).

% The global industry involved in peaceful nuclear energy, including uranium mining, enrichment, and reactor construction. This reading's emphasis on dual-use risk and prohibition would impose significant regulatory and reputational costs, potentially foreclosing parts of their business model.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_industry, excluded,
    powerful, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate global efforts towards complete nuclear disarmament by establishing a clear legal and moral prohibition against nuclear weapons, thereby creating a unified normative framework for abolition.
% TRANSFER_FUNCTION: Transfers the burden of proof and legitimacy from non-proliferation to disarmament, shifting the normative weight from weapon states' security interests to the humanitarian imperative of abolition. It also transfers reputational and legal costs to nuclear weapon states and their allies.
% ABSENT_VOICES: The nuclear industry and states heavily invested in peaceful nuclear energy (even without weapons programs) are largely excluded from the core abolitionist discourse, as their interests are often seen as conflicting with the absolute prohibition norm. They would argue for the continued legitimacy of peaceful nuclear technology under strict safeguards.
% DISAPPEARANCE_RATIONALE: If this abolitionist reading vanished, the normative pressure on nuclear weapon states would significantly decrease, potentially slowing disarmament efforts and re-legitimizing dual-use interpretations of Article IV. The global nuclear governance landscape would revert to a more status-quo, non-proliferation-centric framework.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons and the perceived failure of the NPT to achieve disarmament, leading to a persistent risk of nuclear war and proliferation.
% FOUNDING_PROBLEM_CORROBORATION: Non-nuclear weapon states, civil society organizations, and humanitarian groups consistently attest to the live status of the nuclear threat and the inadequacy of existing frameworks, citing the continued existence of nuclear arsenals and the risk of accidental or intentional use. This corroboration comes from outside the nuclear weapon states themselves.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the significant normative and political costs this reading imposes on nuclear weapon states and those relying on nuclear deterrence. Suppression (0.75) is high because this reading actively seeks to suppress the legitimacy of nuclear weapons and dual-use programs, requiring sustained advocacy and legal challenges against entrenched state interests. The theater ratio (0.4) indicates that while there's genuine normative work, some of the 'disarmament' discourse by weapon states is seen as performative, masking a lack of genuine commitment to abolition. The metrics show a trend of increasing extractiveness and suppression as the abolitionist movement gains traction and challenges the status quo more directly.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of nuclear weapon states, this reading is a snare that seeks to disarm them unilaterally, ignoring their security concerns. From the abolitionist perspective, it is a necessary re-interpretation to achieve global security and uphold humanitarian law. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states and states relying on nuclear deterrence are the primary targets (high d) as this reading directly challenges their core security doctrines and legal justifications. Non-nuclear weapon states advocating for abolition and international humanitarian law advocates are beneficiaries (low d) as this reading aligns with and strengthens their positions. The IAEA is an observer, caught between its mandate to verify peaceful uses (Article IV) and the abolitionist challenge to dual-use legitimacy. The nuclear industry is excluded, as its interests are fundamentally at odds with the absolute prohibition norm.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading implicitly argues that the NPT's original mandate (to prevent proliferation AND achieve disarmament) has suffered mandatrophy on the disarmament side, becoming a cover for continued weapon state possession. The abolitionist reading seeks to resolve this by re-asserting the primacy of disarmament and delegitimizing the current arrangement, preventing the mislabeling of continued weapon possession as 'stable non-proliferation'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dual_use_legitimacy_boundary,
    'Is there a clear and verifiable boundary between ''peaceful'' and ''military'' nuclear programs, or does the dual-use nature of nuclear technology render Article IV inherently problematic from an abolitionist perspective?',
    'Technological advancements in verification or a global consensus on what constitutes ''inherently peaceful'' nuclear technology, independent of weaponization potential.',
    'If no clear boundary exists, the abolitionist reading''s critique of Article IV is strengthened, pushing the NPT further towards delegitimization. If a clear boundary is established, it might allow for a more nuanced abolitionist stance on peaceful nuclear energy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_legitimacy_boundary, conceptual, 'Ambiguity in the dual-use nature of nuclear technology and its implications for Article IV.').

omega_variable(
    tpnw_normative_force,
    'To what extent does the Treaty on the Prohibition of Nuclear Weapons (TPNW) establish a new, binding customary international law norm that overrides or reinterprets the NPT?',
    'Further state ratifications of the TPNW, consistent state practice, and rulings by international courts that affirm the TPNW''s status as customary international law.',
    'Stronger normative force of the TPNW would significantly increase the legal and political costs for nuclear weapon states, reinforcing the abolitionist reading. Weaker normative force would allow weapon states to more easily dismiss the TPNW as a separate, non-binding instrument.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_normative_force, empirical, 'The evolving legal status and normative impact of the TPNW on the NPT regime.').

omega_variable(
    npt_framing_underdetermination,
    'Is the NPT fundamentally a non-proliferation treaty with a disarmament aspiration, or a disarmament treaty with a non-proliferation interim measure?',
    'A definitive, globally accepted interpretive statement or a new treaty that explicitly clarifies the NPT''s primary purpose and the hierarchy of its articles.',
    'If the NPT is primarily a disarmament treaty, the abolitionist reading is structurally vindicated. If it''s primarily a non-proliferation treaty, the abolitionist reading becomes a more radical re-interpretation, facing greater structural resistance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_framing_underdetermination, conceptual, 'The fundamental interpretive framing of the NPT''s core purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.3).
narrative_ontology:measurement(npt__tr_t10, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 10, 0.35).
narrative_ontology:measurement(npt__tr_t20, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 20, 0.38).
narrative_ontology:measurement(npt__tr_t30, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(npt__be_t10, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(npt__be_t20, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 20, 0.83).
narrative_ontology:measurement(npt__be_t30, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(npt__su_t10, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(npt__su_t20, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(npt__su_t30, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_prohibition_norm).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'npt_article_iv_vi_pairing' kernel. It represents the abolitionist interpretation, emphasizing disarmament and the illegitimacy of dual-use programs, and is linked to its sibling readings and the TPNW prohibition norm.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
