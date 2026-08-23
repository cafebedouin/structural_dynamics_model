% ============================================================================
% CONSTRAINT STORY: udhr_authority__customary_emergence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__customary_emergence_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: udhr_authority__customary_emergence_reading
 *   human_readable: UDHR Customary Law Emergence
 *   domain: international_law/human_rights
 *
 * SUMMARY:
 *   This constraint story captures the 'customary emergence' reading of UDHR
 *   authority: the declaration began as a non-binding aspirational document
 *   (1948) but gradually crystallized into binding customary international
 *   law through widespread state practice accompanied by opinio juris (the
 *   belief that such practice is legally obligatory). The reading claims that
 *   key UDHR provisions — particularly the prohibition of torture, genocide,
 *   slavery, and racial discrimination — now bind all states regardless of
 *   treaty ratification. This creates a tangled rope: genuine coordination
 *   (universal baseline rights) combined with asymmetric extraction (states
 *   bound without consent, compliance costs imposed retroactively). The
 *   ambiguous transition point (when exactly did custom crystallize?) creates
 *   strategic interpretive space that institutions and powerful states
 *   exploit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__customary_emergence_reading, 0.55).
domain_priors:suppression_score(udhr_authority__customary_emergence_reading, 0.4).
domain_priors:theater_ratio(udhr_authority__customary_emergence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(udhr_authority__customary_emergence_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__customary_emergence_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__customary_emergence_reading, "UDHR Customary Law Emergence").
narrative_ontology:topic_domain(udhr_authority__customary_emergence_reading, "international_law/human_rights").

domain_priors:requires_active_enforcement(udhr_authority__customary_emergence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__customary_emergence_reading, '5a3a893e-4059-4343-9c1c-2161fa6cd024').
narrative_ontology:cs_kernel_codification('5a3a893e-4059-4343-9c1c-2161fa6cd024', distributed).
narrative_ontology:cs_authority_grounding('5a3a893e-4059-4343-9c1c-2161fa6cd024', practice).
narrative_ontology:cs_interpretation_layer_present('5a3a893e-4059-4343-9c1c-2161fa6cd024').
narrative_ontology:cs_reading_relation('5a3a893e-4059-4343-9c1c-2161fa6cd024', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a3a893e-4059-4343-9c1c-2161fa6cd024', udhr_authority__aspirational_sovereignty_reading, influences).
narrative_ontology:cs_axiom('5a3a893e-4059-4343-9c1c-2161fa6cd024', foundational, customary_law_binds_without_express_consent).
narrative_ontology:cs_axiom_status(customary_law_binds_without_express_consent, holdable).
narrative_ontology:cs_axiom_grounding('5a3a893e-4059-4343-9c1c-2161fa6cd024', customary_law_binds_without_express_consent, conventional).
narrative_ontology:cs_axiom('5a3a893e-4059-4343-9c1c-2161fa6cd024', secondary, opinio_juris_as_psychological_element_of_custom).
narrative_ontology:cs_axiom_status(opinio_juris_as_psychological_element_of_custom, holdable).
narrative_ontology:cs_axiom_grounding('5a3a893e-4059-4343-9c1c-2161fa6cd024', opinio_juris_as_psychological_element_of_custom, conventional).
narrative_ontology:cs_reference_frame('5a3a893e-4059-4343-9c1c-2161fa6cd024', post_war_aspirational_declaration).
narrative_ontology:cs_drift_state('5a3a893e-4059-4343-9c1c-2161fa6cd024', contemporary_customary_law, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5a3a893e-4059-4343-9c1c-2161fa6cd024', '').
narrative_ontology:cs_kernel_id(udhr_authority__customary_emergence_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, international_legal_institutions).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, human_rights_ngos).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, rights_holding_populations).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, non_consenting_states).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, reluctant_compliance_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_authority__customary_emergence_reading, powerful_states).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, powerful_states).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, moderate_states).
narrative_ontology:constraint_victim(udhr_authority__customary_emergence_reading, powerless_states).
narrative_ontology:constraint_vindicates(udhr_authority__customary_emergence_reading, customary_international_law_binds_without_express_consent).
narrative_ontology:constraint_vindicates(udhr_authority__customary_emergence_reading, opinio_juris_as_psychological_element_of_custom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% International courts (ICJ, ICC), treaty bodies, and UN human rights mechanisms administer and interpret the customary law emerging from UDHR. They gain institutional authority and caseload from the binding custom claim. Their exit is analytical — they observe and shape the constraint but are not bound by it as subjects.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, international_legal_institutions, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(udhr_authority__customary_emergence_reading, international_legal_institutions, beneficiary).

% Major powers (US, China, Russia, EU members) bear significant compliance costs when customary obligations conflict with sovereignty preferences, but also benefit from the predictable legal order and can shape customary formation through their practice. Exit is constrained — withdrawal from the international legal order carries massive reputational and strategic costs.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, powerful_states, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__customary_emergence_reading, powerful_states, beneficiary).

% Mid-tier states face compliance costs disproportionate to their influence on customary formation. They participate in state practice and opinio juris but have limited power to shape the emerging norm. Exit is constrained by regional integration and aid dependency.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, moderate_states, payer,
    moderate, biographical, constrained, regional).

% Weak or developing states bear compliance costs with virtually no voice in customary formation. They are bound by norms they had no power to contest during the opinio juris formation period. Exit is effectively trapped — non-compliance invites sanctions, aid cuts, or intervention.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, powerless_states, payer,
    powerless, immediate, trapped, national).

% International and domestic NGOs gain legal leverage, funding justification, and advocacy tools from the customary law reading. They can invoke binding custom in domestic courts and international forums. Exit is mobile — they can shift focus to other frameworks if this one becomes unfavorable.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, human_rights_ngos, beneficiary,
    organized, generational, mobile, global).

% Individuals and groups whose rights are protected by the emerging customary law. They are the nominal beneficiaries but often lack enforcement access. Exit is identity-locked — their rights claims are constituted by this very framework; leaving it means losing the language of entitlement itself.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, rights_holding_populations, beneficiary,
    powerless, biographical, identity_locked, universal).

% Academics and jurists who analyze, critique, and teach the customary emergence thesis. They shape the intellectual environment but do not directly bear compliance costs or collect enforcement benefits. Their seat is analytical by structure.
narrative_ontology:constraint_stakeholder(udhr_authority__customary_emergence_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, universal baseline of human rights obligations that coordinates state behavior without requiring each state to individually ratify every treaty. Solves the coordination problem of how to establish common standards in a system of formally sovereign equals.
% TRANSFER_FUNCTION: Moves compliance costs (legislative reform, judicial enforcement, reporting obligations, reparations) from rights-holding populations to states, particularly states that did not expressly consent to the specific obligations. The transfer is justified by the claim that customary law binds regardless of consent.
% ABSENT_VOICES: States that persistently objected during customary formation but were overridden by majority practice; indigenous peoples and non-state communities whose consent was never sought in the opinio juris calculus; future generations who inherit the customary obligations without participating in their formation.
% DISAPPEARANCE_RATIONALE: If the customary emergence reading vanished, states would revert to a pure consent-based system (treaty-only obligations). Compliance costs would drop for non-ratifying states; protections would weaken for populations in non-ratifying states; international courts would lose jurisdiction over non-treaty-based claims. The entire post-1948 human rights architecture would restructure around express consent.
% FOUNDING_PROBLEM: The post-WWII need for universal human rights standards that could bind all states, including those that refused to ratify specific treaties, without requiring a world legislature or universal consent.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UDHR preamble and contemporaneous UN records (outside beneficiary confirmation). However, the customary emergence thesis itself is contested: the ILC's 2018 Conclusions on Customary International Law and ICJ jurisprudence (e.g., Nicaragua v. USA) corroborate the mechanism, while persistent objector doctrine and state practice of non-compliance corroborate the contestation.
narrative_ontology:disappearance_verdict(udhr_authority__customary_emergence_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__customary_emergence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__customary_emergence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__customary_emergence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__customary_emergence_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__customary_emergence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__customary_emergence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__customary_emergence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 (1948, purely aspirational) to 0.55 (2024, substantial customary obligations) as more provisions achieve customary status and enforcement mechanisms multiply. Suppression is moderate (0.4) — states are not physically coerced but face diplomatic, economic, and legal consequences for non-compliance. Theater ratio is low-moderate (0.25) — the coordination function is real (universal standards facilitate cooperation) but performative compliance exists (states ratify treaties but undermine implementation). Accessibility collapse at 0.5 reflects that while states can theoretically persistently object, the political cost is prohibitive for most. Resistance at 0.45 captures ongoing state pushback (reservations, non-compliance, sovereign immunity claims) that falls short of rejecting the framework entirely.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat, the constraint appears as genuine coordination solving a collective action problem. From the powerless state seat, it appears as extraction without representation. From the rights-holder seat, it appears as essential protection. The engine computes these per-seat classifications from the structural data — the claimed type (tangled_rope) reflects the author's judgment that both coordination and extraction are structurally present, not that one seat's view is 'correct.'
 *
 * DIRECTIONALITY LOGIC:
 *   International legal institutions are agenda-setters with analytical exit — they shape the constraint but don't bear its costs. Powerful states are dual-role: they pay compliance costs but also benefit from legal order and shape custom formation (constrained exit). Moderate and powerless states are primarily payers with constrained/trapped exit — they bear costs disproportionate to influence. NGOs are beneficiaries with mobile exit. Rights-holding populations are beneficiaries but identity-locked — their entitlements are constituted by this framework. The extractiveness is experienced differently: for powerless states, χ is high (trapped, high d); for powerful states, χ is moderated by their shaping power (constrained exit, lower d); for institutions, χ is negative (subsidy, d near 0).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal binding standards without universal consent) remains contested — not dead, because treaty gaps persist; not live in its original form, because the treaty system has expanded dramatically. The customary emergence reading prevents mislabeling: it is not a pure snare (coordination is real), not a pure rope (consent deficit is real), not a scaffold (no sunset, no transition endpoint declared). The mandatrophy risk is that the customary mechanism becomes a ratchet — only adding obligations, never subtracting — which the measurement series shows (rising extractiveness).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_transition_point,
    'When exactly did UDHR provisions crystallize into customary international law, and is there a single transition point or a staggered process per provision?',
    'ICJ jurisprudence tracking first recognition of specific provisions as customary; ILC conclusions on identification of customary law; state practice databases showing opinio juris emergence timelines per right.',
    'A single early transition point (e.g., 1960s) maximizes extraction (decades of retroactive binding). A staggered late transition (1990s+) minimizes extraction. The ambiguity itself is the strategic interpretive space.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_transition_point, empirical, 'The temporal ambiguity of customary crystallization — the core strategic variable.').

omega_variable(
    persistent_objector_effectiveness,
    'Can a state that persistently objects during customary formation actually avoid being bound, or is the persistent objector doctrine a theoretical escape hatch that fails in practice?',
    'Empirical study of states that claimed persistent objection to specific UDHR-derived norms (e.g., US on death penalty, Islamic states on gender equality provisions) and whether they were treated as bound in ICJ/UN practice.',
    'If persistent objection fails systematically, the constraint is more snare-like (no exit). If it succeeds, the constraint has a genuine coordination escape valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistent_objector_effectiveness, empirical, 'Whether the theoretical exit option for states is practically available.').

omega_variable(
    beneficiary_capture_of_customary_formation,
    'Do international legal institutions and powerful states actively shape state practice and opinio juris to expand customary obligations in directions that serve their interests?',
    'Process-tracing of ICJ advisory opinions, ILC topic selection, UN treaty body general comments, and powerful state diplomatic practice to identify systematic bias in customary development.',
    'If beneficiary capture is documented, the coordination function is compromised — the constraint becomes more snare-like. If customary formation is relatively autonomous, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_customary_formation, conceptual, 'Whether the customary formation process itself is captured by the constraint''s beneficiaries.').

omega_variable(
    kernel_reading_identity,
    'This constraint is the customary_emergence_reading of the udhr_authority kernel. What structural elements distinguish it from the binding_universalism_reading and aspirational_sovereignty_reading?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, temporal profiles, and enforcement mechanisms. The binding_universalism_reading has higher extractiveness (immediate justiciability), the aspirational_sovereignty_reading has near-zero extractiveness (consent-gated).',
    'If readings collapse into each other structurally, the kernel decomposition is invalid. If they maintain distinct structural profiles, the decomposition is warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the kernel reading decomposition — this reading''s structural distinctness from siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__customary_emergence_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_customary_tr_t1948, udhr_authority__customary_emergence_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_customary_tr_t1966, udhr_authority__customary_emergence_reading, theater_ratio, 1966, 0.12).
narrative_ontology:measurement(udhr_customary_tr_t1976, udhr_authority__customary_emergence_reading, theater_ratio, 1976, 0.15).
narrative_ontology:measurement(udhr_customary_tr_t1990, udhr_authority__customary_emergence_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(udhr_customary_tr_t2005, udhr_authority__customary_emergence_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(udhr_customary_tr_t2024, udhr_authority__customary_emergence_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(udhr_customary_be_t1948, udhr_authority__customary_emergence_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(udhr_customary_be_t1966, udhr_authority__customary_emergence_reading, base_extractiveness, 1966, 0.25).
narrative_ontology:measurement(udhr_customary_be_t1976, udhr_authority__customary_emergence_reading, base_extractiveness, 1976, 0.3).
narrative_ontology:measurement(udhr_customary_be_t1990, udhr_authority__customary_emergence_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(udhr_customary_be_t2005, udhr_authority__customary_emergence_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(udhr_customary_be_t2024, udhr_authority__customary_emergence_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(udhr_customary_su_t1948, udhr_authority__customary_emergence_reading, suppression_requirement, 1948, 0.1).
narrative_ontology:measurement(udhr_customary_su_t1966, udhr_authority__customary_emergence_reading, suppression_requirement, 1966, 0.2).
narrative_ontology:measurement(udhr_customary_su_t1976, udhr_authority__customary_emergence_reading, suppression_requirement, 1976, 0.25).
narrative_ontology:measurement(udhr_customary_su_t1990, udhr_authority__customary_emergence_reading, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(udhr_customary_su_t2005, udhr_authority__customary_emergence_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(udhr_customary_su_t2024, udhr_authority__customary_emergence_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__customary_emergence_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__customary_emergence_reading, 0.1).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, udhr_authority__aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, icc_rome_statute_obligations).
narrative_ontology:affects_constraint(udhr_authority__customary_emergence_reading, icat_torture_convention_customary_extension).

% DUAL FORMULATION NOTE:
% Part of the udhr_authority kernel family. This reading (customary_emergence) occupies the middle ground between binding_universalism (high extractiveness, immediate justiciability) and aspirational_sovereignty (near-zero extractiveness, consent-gated). The three readings share the UDHR text as kernel but instantiate different constraints with different ε, different stakeholder structures, and different temporal trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__customary_emergence_reading, institutional, 0.1).
constraint_indexing:directionality_override(udhr_authority__customary_emergence_reading, powerful, 0.35).
constraint_indexing:directionality_override(udhr_authority__customary_emergence_reading, moderate, 0.65).
constraint_indexing:directionality_override(udhr_authority__customary_emergence_reading, powerless, 0.85).
constraint_indexing:directionality_override(udhr_authority__customary_emergence_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
