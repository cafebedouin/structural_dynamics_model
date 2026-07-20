% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA Binding Multilateral Treaty Reading
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint story instantiates the binding_multilateral_reading of
 *   the jcpoa_treaty_bindingness kernel. It treats the Joint Comprehensive
 *   Plan of Action not as a reversible bargain but as a binding multilateral
 *   instrument anchored in UNSC Resolution 2231, requiring consensus for
 *   modification or dissolution and channeling disputes through the Joint
 *   Commission. The reading forecloses unilateral withdrawal and locates the
 *   JCPOA's authority in a formalized, lineage-grounded commitment system.
 *
 * KEY AGENTS:
 *   - joint_commission: agenda_setter and institutional beneficiary â administers consensus dispute resolution and procurement channel
 *   - p5_plus_one_states: primary beneficiaries â receive non-proliferation stability and are constrained from unilateral snapback
 *   - iaea_secretariat: institutional beneficiary â receives expanded verification authority and mandate
 *   - iran: primary payer â bears concrete sovereign constraints on enrichment and reactor capacity
 *   - regional_security_actors: excluded parties â bear security externalities without Joint Commission representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.45).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA Binding Multilateral Treaty Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '1383dcbc-00d7-4f60-bafc-8f19c8eaeede').
narrative_ontology:cs_kernel_codification('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', formalized).
narrative_ontology:cs_authority_grounding('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', lineage).
narrative_ontology:cs_interpretation_layer_present('1383dcbc-00d7-4f60-bafc-8f19c8eaeede').
narrative_ontology:cs_reading_relation('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', foundational, unilateral_modification_foreclosed).
narrative_ontology:cs_axiom_status(unilateral_modification_foreclosed, holdable).
narrative_ontology:cs_axiom_grounding('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', unilateral_modification_foreclosed, conventional).
narrative_ontology:cs_axiom('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', foundational, multilateral_dispute_resolution_mandatory).
narrative_ontology:cs_axiom_status(multilateral_dispute_resolution_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', multilateral_dispute_resolution_mandatory, conventional).
narrative_ontology:cs_reference_frame('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', multilateral_consensus_framework).
narrative_ontology:cs_drift_state('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', post_us_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1383dcbc-00d7-4f60-bafc-8f19c8eaeede', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_one_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_secretariat).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, joint_commission).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers JCPOA implementation through consensus-based decision-making, oversees the procurement channel, and convenes dispute resolution under the binding multilateral framework. Its institutional authority derives directly from the agreement's formalized structure and UNSC Resolution 2231.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, joint_commission, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, joint_commission, beneficiary).

% Receive verified Iranian nuclear rollback and non-proliferation regime stability. Under the binding reading, their capacity to reimpose sanctions unilaterally is structurally constrained by the consensus requirement, making the commitment symmetrically locking.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_one_states, beneficiary,
    powerful, generational, constrained, global).

% Derives expanded verification authority, access protocols, and budgetary mandate from the JCPOA's binding monitoring arrangements. Its technical reports feed the Joint Commission's consensus dispute resolution process.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_secretariat, beneficiary,
    institutional, generational, constrained, global).

% Bears the primary sovereign constraint: enrichment capped at 3.67 percent, Arak reactor reconfigured, Fordow converted, Additional Protocol applied. Cannot unilaterally modify obligations; exit risks automatic snapback of UNSC sanctions and loss of sanctions relief.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer,
    moderate, generational, constrained, national).

% Israel, Saudi Arabia, and other regional states whose security is directly affected by Iranian nuclear capacity but who hold no seat in the Joint Commission or IAEA governance. They would argue for zero enrichment or military alternatives but are structurally absent from consensus procedures.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_security_actors, excluded,
    powerful, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__binding_multilateral_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__binding_multilateral_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents Iranian nuclear weaponization through verified enrichment limits, reactor modifications, and enhanced monitoring, while providing a multilateral dispute resolution and sanctions relief architecture that replaces unilateral military escalation with coordinated diplomacy.
% TRANSFER_FUNCTION: Moves Iranian sovereign nuclear capacityâenrichment levels, stockpiles, reactor designsâinto monitored constraint, in exchange for multilateral sanctions relief and binding restrictions on unilateral sanctions reimposition by the P5+1.
% ABSENT_VOICES: Regional actors such as Israel and Saudi Arabia, and Iranian domestic hardliners, are excluded from Joint Commission consensus but bear the security consequences; they would argue for either stricter enforcement or sovereign nuclear rights.
% DISAPPEARANCE_RATIONALE: If the binding consensus architecture vanished overnight, the sanctions-relief-for-restraints trade would collapse, unilateral sanctions and military threat cycles would resume, and the IAEA's expanded verification mandate would lose its legal foundationâthe non-proliferation regime would reorganize around coercive unilateralism.
% FOUNDING_PROBLEM: The Iranian nuclear program had advanced to near-breakout capability, unilateral sanctions had failed to halt enrichment, and military options risked regional war; a binding multilateral agreement was needed to verifiably cap the program and replace coercive unilateralism with coordinated relief.
% FOUNDING_PROBLEM_CORROBORATION: IAEA Directors General independently corroborated the verification value. Regional actors outside the P5+1 (Israel, Saudi Arabia) dispute that the founding problem is solved by this arrangement, asserting it merely delays breakout. Iranian civil society and technical experts dispute the legitimacy of the sovereign concession. No unanimity; corroboration is partial and seat-dependent.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial sovereign capacity Iran commits to the constraint against comparatively diffuse and reversible sanctions relief. Suppression (0.45) is moderate: the consensus requirement suppresses unilateral alternatives but did not prevent the 2018 US withdrawal. Theater_ratio (0.50 at interval end) captures the increasing performative maintenance of the binding framework after US withdrawal and Iranian gradual compliance reduction â Joint Commission meetings continue while functional constraint erodes. Accessibility_collapse (0.70) is high because once the binding multilateral frame is accepted, unilateral military or sanctions alternatives are delegitimized within diplomatic discourse. Resistance (0.40) reflects Iranian hardliner opposition, US Congressional skepticism, and regional actor rejection.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian seat experiences the constraint as direct sovereign extraction with limited exit, computing toward high effective extraction. The P5+1 and institutional seats experience it as coordination that stabilizes the non-proliferation regime and constrains their own unilateral impulses, computing toward low or negative effective extraction. The divergence is structural: the same consensus rule that protects Iran from unilateral snapback also locks in its enrichment constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the P5+1 states and the IAEA/Joint Commission institutions that gain stability, verification authority, and a diplomatic forum. Victim declaration maps to Iran as the sole party bearing concrete, irreversible sovereign concessions. The asymmetry is deliberate: one side gives up centrifuges and reactor cores; the other gives up sanctions that could be reimposed anyway. This drives the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â uncontrolled Iranian nuclear advance and the failure of unilateral sanctions â was live in 2015 but became contested after 2018 when the US withdrew and Iran expanded enrichment beyond JCPOA limits. The framework persists because the diplomatic and verification infrastructure would collapse without it, yet its functional capacity to constrain Iranian nuclear activity has degraded. The binding multilateral reading prevents mislabeling this as a simple Snare by insisting on the genuine coordination function (non-proliferation verification, dispute resolution) that persists even as extraction asymmetries remain. It also prevents mislabeling it as pure Rope by acknowledging the sovereign cost borne asymmetrically by Iran.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_legal_character,
    'Does UNSC Resolution 2231 and the JCPOA text create legally binding obligations under international law, or a political commitment whose ''bindingness'' is merely performative diplomacy?',
    'International Court of Justice advisory opinion or authoritative UNSC legal interpretation clarifying whether the resolution''s language creates treaty-level obligations or non-binding arrangements.',
    'If non-binding, this reading collapses into the transactional_provisional_reading; if binding, the consensus architecture has supranational force and the directionality toward Iran is locked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_legal_character, conceptual, 'Legal character of the JCPOA under international law').

omega_variable(
    unilateral_withdrawal_vclt,
    'Does the Vienna Convention on the Law of Treaties permit unilateral withdrawal from the JCPOA despite the binding multilateral reading''s consensus requirement?',
    'Adjudication by the ICJ or authoritative treaty interpretation establishing whether the JCPOA is a treaty under VCLT Article 2 and whether withdrawal clauses are implied or excluded.',
    'If VCLT withdrawal is available, the high constraint on unilateral withdrawal is structurally illusory and suppression is lower than authored; if excluded, the bindingness is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_vclt, conceptual, 'Availability of unilateral withdrawal under general treaty law').

omega_variable(
    snapback_consensus_veto,
    'Does the consensus requirement for sanctions snapback function as multilateral governance or as a structural veto protecting Iranian non-compliance?',
    'Empirical analysis of Joint Commission voting behavior if Iran were found in significant non-compliance: would consensus form, or would permanent member vetoes block snapback?',
    'If consensus is impossible in practice, the binding reading''s enforcement mechanism is theater and the constraint drifts toward piton; if consensus is achievable, the multilateral governance is real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(snapback_consensus_veto, empirical, 'Functional viability of consensus-based snapback').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_bind_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpoa_bind_tr_t1, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 1, 0.17).
narrative_ontology:measurement(jcpoa_bind_tr_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(jcpoa_bind_tr_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement(jcpoa_bind_tr_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 4, 0.35).
narrative_ontology:measurement(jcpoa_bind_tr_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 5, 0.38).
narrative_ontology:measurement(jcpoa_bind_tr_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement(jcpoa_bind_tr_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 7, 0.45).
narrative_ontology:measurement(jcpoa_bind_tr_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 8, 0.48).
narrative_ontology:measurement(jcpoa_bind_tr_t9, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 9, 0.5).

% Extraction over time
narrative_ontology:measurement(jcpoa_bind_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jcpoa_bind_be_t1, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 1, 0.46).
narrative_ontology:measurement(jcpoa_bind_be_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(jcpoa_bind_be_t3, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(jcpoa_bind_be_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(jcpoa_bind_be_t5, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(jcpoa_bind_be_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(jcpoa_bind_be_t7, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 7, 0.62).
narrative_ontology:measurement(jcpoa_bind_be_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(jcpoa_bind_be_t9, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 9, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jcpoa_treaty_bindingness__binding_multilateral_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jcpoa_treaty_bindingness kernel. The binding_multilateral_reading, transactional_provisional_reading, and graduated_compliance_reading are structurally distinct constraints with different epsilon values, beneficiary/victim structures, and directionality profiles. They share a diplomatic-historical referent but instantiate different legal and political architectures. Network edges reflect their membership in a constraint family rather than causal influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
