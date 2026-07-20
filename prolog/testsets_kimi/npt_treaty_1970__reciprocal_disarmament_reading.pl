% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Reciprocal Disarmament Reading (Article VI Binding)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The NPT is widely presented as a grand bargain: non-nuclear-weapon states
 *   forgo nuclear weapons and accept intrusive safeguards, while
 *   nuclear-weapon states pursue disarmament under Article VI. This
 *   constraint story instantiates the reciprocal_disarmament_reading of the
 *   npt_treaty_1970 kernel, which treats Article VI as a binding legal
 *   obligation with temporal urgency, not mere aspiration. Under this
 *   reading, the treaty's horizontal nonproliferation rules and vertical
 *   disarmament commitments form a single reciprocal structure. The
 *   enforcement gap â full IAEA verification of non-nuclear-weapon-state
 *   obligations, zero verification of nuclear-weapon-state disarmament â
 *   becomes a structural injustice rather than an implementation detail.
 *   Nuclear-weapon states experience the constraint as a limit on strategic
 *   autonomy (victim seat), while the non-nuclear-weapon-state coalition
 *   gains normative leverage to demand compliance (beneficiary seat). The
 *   constraint is claimed as tangled_rope: it coordinates the global
 *   nonproliferation order, but the asymmetry of enforceable obligations
 *   extracts from the nuclear-weapon-state side of the bargain while
 *   delivering unverified promises to the non-nuclear-weapon-state side.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary target/victim (powerful/institutional, constrained exit) â bears extraction through constrained modernization and political costs.
 *   - non_nuclear_weapon_states: Primary beneficiary (organized, constrained exit) â gains normative leverage from Article VI.
 *   - international_atomic_energy_agency: Secondary institutional actor (institutional, constrained) â enforces the asymmetric verification architecture.
 *   - withdrawal_claimant_states: Excluded voice (moderate, trapped) â would assert Article X sovereignty against the reciprocal-bargain frame.
 *   - disarmament_advocacy_complex: Analytical observer (organized, mobile) â tracks compliance gaps and amplifies non-nuclear-weapon-state leverage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Reciprocal Disarmament Reading (Article VI Binding)").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '180b430c-fd9f-400c-a888-e13b0cb4c25d').
narrative_ontology:cs_kernel_codification('180b430c-fd9f-400c-a888-e13b0cb4c25d', formalized).
narrative_ontology:cs_authority_grounding('180b430c-fd9f-400c-a888-e13b0cb4c25d', lineage).
narrative_ontology:cs_interpretation_layer_present('180b430c-fd9f-400c-a888-e13b0cb4c25d').
narrative_ontology:cs_reading_relation('180b430c-fd9f-400c-a888-e13b0cb4c25d', npt_treaty_1970__oligopoly_enforcement_reading, forecloses).
narrative_ontology:cs_reading_relation('180b430c-fd9f-400c-a888-e13b0cb4c25d', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('180b430c-fd9f-400c-a888-e13b0cb4c25d', foundational, article_vi_binding_temporal_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_temporal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('180b430c-fd9f-400c-a888-e13b0cb4c25d', article_vi_binding_temporal_obligation, conventional).
narrative_ontology:cs_axiom('180b430c-fd9f-400c-a888-e13b0cb4c25d', foundational, horizontal_vertical_reciprocity).
narrative_ontology:cs_axiom_status(horizontal_vertical_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('180b430c-fd9f-400c-a888-e13b0cb4c25d', horizontal_vertical_reciprocity, conventional).
narrative_ontology:cs_reference_frame('180b430c-fd9f-400c-a888-e13b0cb4c25d', reciprocal_bargain_equilibrium).
narrative_ontology:cs_drift_state('180b430c-fd9f-400c-a888-e13b0cb4c25d', contemporary_npt_stress, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('180b430c-fd9f-400c-a888-e13b0cb4c25d', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals and sit as the five recognized nuclear-weapon states under the NPT. They administer the horizontal nonproliferation regime and dominate the Review Conference process. Under this reading, they bear a binding legal obligation under Article VI to pursue disarmament, which generates political costs and constrains strategic modernization, even as they continue to modernize delivery systems.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer).

% Comprise the vast majority of NPT states parties. They have forsworn nuclear weapons and accepted full-scope IAEA safeguards. Under this reading, they gain normative leverage from Article VI to demand verified disarmament from the nuclear-weapon states, converting the treaty text into a reciprocal bargaining instrument despite the absence of Article VI verification.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).

% Administers the safeguards system that verifies non-nuclear-weapon-state compliance with nonproliferation obligations. Its mandate stops at the threshold of nuclear-weapon-state facilities because no verification mechanism exists for Article VI, making its enforcement function structurally asymmetric.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Monitors and campaigns for Article VI compliance through shadow reporting, treaty monitoring, and public advocacy. They amplify the normative leverage of the non-nuclear-weapon-state coalition and document the gap between disarmament rhetoric and modernization practice.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, disarmament_advocacy_complex, observer,
    organized, biographical, mobile, global).

% States that have invoked or contemplate invoking Article X withdrawal. They are structurally excluded from the reciprocal-bargain framing because their exit is treated as defection rather than legitimate sovereignty, even though the treaty text permits withdrawal under security imperatives.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, withdrawal_claimant_states, excluded,
    moderate, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation by linking non-nuclear-weapon-state renunciation of nuclear weapons to nuclear-weapon-state pursuit of disarmament, creating a global reciprocal bargain intended to stabilize the nuclear order.
% TRANSFER_FUNCTION: Transfers strategic autonomy (modernization and force-posture options) from nuclear-weapon states to the collective nonproliferation regime, and transfers normative leverage over disarmament discourse from the treaty text to the non-nuclear-weapon-state coalition.
% ABSENT_VOICES: Withdrawal-claimant states and nuclear-modernization constituencies within nuclear-weapon states would object: the former to the framing of Article X as defection rather than sovereignty, the latter to the characterization of strategic modernization as unlawful victimhood. They are absent from Review Conference consensus documents.
% DISAPPEARANCE_RATIONALE: If the reciprocal-bargain reading vanished, the NPT would revert to a horizontal nonproliferation oligopoly. Non-nuclear-weapon states would lose the legal basis to demand disarmament; nuclear-weapon states would face reduced political costs for modernization; the TPNW and related disarmament architecture would lose their doctrinal anchor.
% FOUNDING_PROBLEM: The unchecked proliferation of nuclear weapons to multiple states in the 1960s threatened catastrophic war and destabilized the post-war order; a treaty was needed to cap the number of nuclear powers while promising eventual disarmament to secure non-nuclear-state accession.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars outside the nuclear-weapon-state coalition corroborate that the NPT was founded as a reciprocal grand bargain. Nuclear-weapon-state foreign ministries assert the nonproliferation problem remains live, while non-nuclear-weapon-state diplomats and independent disarmament monitors contest that the original reciprocal design is being honored.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the reciprocal bargain is one-sided in enforcement: non-nuclear-weapon-state obligations are verified and immediate, while nuclear-weapon-state disarmament is unverified and deferred. Suppression (0.62) reflects the active diplomatic and normative machinery that prevents non-nuclear-weapon states from exiting their safeguards and nuclear-weapon states from openly repudiating Article VI. Theater ratio (0.72) is high because nuclear-weapon-state disarmament reporting has become largely performative (stockpile transparency exercises, ritual reaffirmations at Review Conferences) while modernization programs continue. Accessibility collapse (0.78) is high because the NPT enjoys near-universal membership and alternatives (proliferation, Article X withdrawal) are heavily stigmatized. Resistance (0.58) captures active non-nuclear-weapon-state coalition pressure and the TPNW challenge, offset by nuclear-weapon-state refusal to comply.
 *
 * PERSPECTIVAL GAP:
 *   From the non-nuclear-weapon-state seat, the constraint is a legitimate enforcement mechanism of a reciprocal bargain; from the nuclear-weapon-state seat, it is a rhetorical trap that constrains modernization without delivering corresponding benefits. The IAEA seat sees a technical safeguards mandate that stops at the threshold of nuclear-weapon-state facilities. The engine computes these divergences from the structural declarations: non-nuclear-weapon states are beneficiaries with constrained exit, nuclear-weapon states are victims with constrained exit but high power.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-nuclear-weapon states are declared beneficiaries because they gain normative leverage and security from the nonproliferation regime. Nuclear-weapon states are declared victims because their strategic autonomy is constrained by Article VI obligations that generate real political costs (Review Conference bargaining, TPNW pressure) despite their power. The high power of nuclear-weapon states dampens their effective extraction, but the victim declaration places them at the target end of the directionality spectrum. The IAEA is neither beneficiary nor victim; its directionality is analytically neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing unchecked nuclear proliferation â is contested but not dead. The constraint is not a piton because it delivers real coordination (near-zero non-nuclear-weapon-state proliferation for 50+ years) and has live beneficiaries (non-nuclear-weapon-state leverage). It is not a snare because the coordination function is not cover: the nonproliferation success is genuine. It is a tangled_rope because the same structure that coordinates also extracts asymmetrically through the Article VI enforcement gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a binding legal obligation with temporal urgency, or an aspirational goal contingent on strategic conditions?',
    'ICJ jurisprudence, state practice analysis, and treaty interpretation under the Vienna Convention on the Law of Treaties.',
    'If aspirational, the constraint''s victim set collapses and it reclassifies toward oligopoly enforcement; if binding, the asymmetry is structurally unjust and the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'Ambiguity in the legal character of Article VI.').

omega_variable(
    nws_victimhood_plausibility,
    'Can nuclear-weapon states â the most powerful actors in the system â be plausibly classified as structural victims of a treaty they designed and administer?',
    'Directionality computation review: assess whether high power and global scope dampen effective extraction below the threshold required for victim classification.',
    'If power neutralizes extraction, the nuclear-weapon-state seat computes as beneficiary/agenda-setter and the constraint may reclassify as rope or snare depending on non-nuclear-weapon-state position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_victimhood_plausibility, conceptual, 'Whether power asymmetry invalidates nuclear-weapon-state victim status.').

omega_variable(
    enforcement_gap_nature,
    'Does the absence of Article VI verification reflect a contingent implementation gap or a structurally intended asymmetry?',
    'Historical treaty negotiation records; analysis of nuclear-weapon-state bargaining power at the NPT''s founding.',
    'If structurally intended, the constraint''s extractiveness is baked into its design; if contingent, reform is theoretically possible and the classification leans toward scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_gap_nature, empirical, 'Whether the Article VI enforcement gap is designed or incidental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 40, 0.62).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 50, 0.7).
narrative_ontology:measurement(npt__tr_t54, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 54, 0.72).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt__be_t10, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(npt__be_t20, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(npt__be_t30, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(npt__be_t40, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(npt__be_t50, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(npt__be_t54, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 54, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(npt__su_t10, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(npt__su_t20, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(npt__su_t30, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(npt__su_t40, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 40, 0.58).
narrative_ontology:measurement(npt__su_t50, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(npt__su_t54, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 54, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The NPT treaty text is a contested kernel that decomposes into at least three structurally distinct constraints. This story instantiates the reciprocal_disarmament_reading, which treats Article VI as a binding obligation creating a reciprocal bargain. The oligopoly_enforcement_reading treats Article VI as aspirational and Articles I-II as the binding core. The withdrawal_sovereignty_reading treats Article X as a legitimate sovereignty safety valve. These are not observational variants of one constraint but distinct normative commitments with different beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
