% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 Expansive Human Rights Reading
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   This constraint story captures the expansive human rights reading of
 *   Common Article 3 of the Geneva Conventions. This reading asserts that CA3
 *   applies to any organized armed violence, establishing a floor of minimum
 *   humanitarian standards irrespective of how the conflict is classified
 *   (international, non-international, or lower-intensity). The reading
 *   extends the constraint's scope broadly, bringing all detainees and
 *   affected populations into the victim set and subjecting state security
 *   operations to external monitoring and potential prosecution. The
 *   constraint is a legal norm, not a natural law, and its operation involves
 *   genuine coordination (humanitarian protection) and asymmetric extraction
 *   (compliance costs borne by parties to the conflict). The claimed type is
 *   tangled_rope, reflecting both functions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.6).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.5).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 Expansive Human Rights Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "legal/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '328d9a6d-1da3-4c55-a6a8-17146dc763cd').
narrative_ontology:cs_kernel_codification('328d9a6d-1da3-4c55-a6a8-17146dc763cd', formalized).
narrative_ontology:cs_authority_grounding('328d9a6d-1da3-4c55-a6a8-17146dc763cd', lineage).
narrative_ontology:cs_interpretation_layer_present('328d9a6d-1da3-4c55-a6a8-17146dc763cd').
narrative_ontology:cs_reading_relation('328d9a6d-1da3-4c55-a6a8-17146dc763cd', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('328d9a6d-1da3-4c55-a6a8-17146dc763cd', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('328d9a6d-1da3-4c55-a6a8-17146dc763cd', foundational, ca3_applies_to_all_organized_armed_violence).
narrative_ontology:cs_axiom_status(ca3_applies_to_all_organized_armed_violence, holdable).
narrative_ontology:cs_axiom_grounding('328d9a6d-1da3-4c55-a6a8-17146dc763cd', ca3_applies_to_all_organized_armed_violence, deontological).
narrative_ontology:cs_axiom('328d9a6d-1da3-4c55-a6a8-17146dc763cd', foundational, humanitarian_floor_non_derogable).
narrative_ontology:cs_axiom_status(humanitarian_floor_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('328d9a6d-1da3-4c55-a6a8-17146dc763cd', humanitarian_floor_non_derogable, deontological).
narrative_ontology:cs_reference_frame('328d9a6d-1da3-4c55-a6a8-17146dc763cd', teleological_ca3_interpretation).
narrative_ontology:cs_drift_state('328d9a6d-1da3-4c55-a6a8-17146dc763cd', contemporary_human_rights_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('328d9a6d-1da3-4c55-a6a8-17146dc763cd', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detainees).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, persons_hors_de_combat).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_armed_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_operations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_governments).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, common_article_3_universal_application).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, humanitarian_floor_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty in connection with organized armed violence; they have no exit from the situation and depend entirely on the constraint for minimum protections.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detainees, beneficiary,
    powerless, immediate, trapped, global).

% Civilians affected by organized armed violence; they cannot exit the conflict zone easily and rely on the constraint for protection from violence and deprivation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations, beneficiary,
    powerless, immediate, constrained, global).

% National military forces engaged in organized armed violence; they bear the cost of compliance with humanitarian standards (training, restraint, detention conditions) and face prosecution for violations. Exit from the constraint is constrained by the nature of their mission and international law.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_armed_forces, payer,
    institutional, generational, constrained, global).

% Organized non-state armed groups engaged in sustained violence; they bear compliance costs and face potential prosecution, but have less capacity to implement standards and fewer exit options.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, global).

% Governments that have ratified the Geneva Conventions; they set the agenda for treaty interpretation and bear compliance costs through their armed forces and security operations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_governments, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, state_governments, payer).

% The International Committee of the Red Cross monitors compliance, promotes IHL, and acts as guardian of the Geneva Conventions; it sets the agenda for humanitarian protection but does not bear compliance costs.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, ircrc, agenda_setter,
    institutional, generational, analytical, global).

% International and regional human rights courts (e.g., ECtHR, IACtHR) adjudicate violations of humanitarian standards; they enforce the constraint through jurisprudence.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_human_rights_courts, agenda_setter,
    institutional, generational, analytical, global).

% Academic experts who analyze and critique the interpretation and application of Common Article 3; they influence interpretive communities but hold no enforcement power.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal floor of minimum humanitarian protections (humane treatment, prohibition of torture, fair trial guarantees) applicable to all organized armed violence, ensuring baseline protections regardless of conflict classification.
% TRANSFER_FUNCTION: Moves the burden of compliance (resources for humane detention, restraint in operations, judicial guarantees) from protected persons to parties to the conflict (states and armed groups), and moves accountability (monitoring, prosecution) to international and domestic mechanisms.
% ABSENT_VOICES: Proponents of the state-centric reading (who argue CA3 applies only above a threshold of intensity) and the ICRC customary reading (who tie scope to state practice) are not represented in the constraint's operational seats; they operate in the interpretive sphere but their exclusion from the beneficiary/payer structure means their objections do not shape the constraint's enforcement.
% DISAPPEARANCE_RATIONALE: If the expansive reading vanished overnight, states and armed groups would revert to narrower interpretations, reducing protections for detainees and civilians in lower-intensity conflicts, and international monitoring/prosecution would lose its legal basis for those situations.
% FOUNDING_PROBLEM: The need to ensure minimum humanitarian protections in non-international armed conflicts and other organized violence where the full Geneva Conventions do not apply, preventing a protection gap for victims.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and human rights treaty bodies attest the protection gap persists; states and some scholars contest whether the gap exists or whether the expansive reading overreaches.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.6, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.6) reflects significant compliance costs for states and armed groups (training, detention standards, judicial guarantees, restraint in operations). Suppression (0.5) captures the coercive enforcement of the norm through international monitoring, judicial mechanisms, and the suppression of alternative interpretations that would limit scope. Theater ratio (0.3) indicates some performative compliance but largely functional enforcement. Accessibility collapse (0.4) is moderate: alternatives exist (narrower readings) but the expansive reading reduces exit options for parties to the conflict. Resistance (0.6) reflects persistent state pushback against broad application.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (detainees, civilians) experience the constraint as a protective mountain-like guarantee — high accessibility collapse, near-zero resistance from their side. The payer seats (state armed forces, non-state groups) experience it as an extractive snare-like burden — high resistance, constrained exit. The agenda-setter seats (ICRC, courts) see it as a coordination rope they administer. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (detainees, civilians) are structural beneficiaries: the constraint subsidizes their protection (d near 0.0). Payers (state armed forces, non-state groups, state security operations) are structural targets: the constraint extracts compliance costs and exposes them to prosecution (d near 1.0). Agenda-setters (governments, ICRC, courts) sit near symmetric (d ~0.5): they both shape and are bound by the norm. Exit options differentiate: detainees are trapped; armed groups are constrained; courts are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection gap in non-international conflicts) remains live. The constraint has not atrophied; its scope has expanded via human rights jurisprudence. No mandatrophy resolution is declared. The constraint continues to serve its coordination function while extraction has increased as enforcement mechanisms strengthened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the expansive human rights reading a distinct constraint from the state-centric and ICRC customary readings, or are they competing interpretations of the same constraint?',
    'Apply the ε-invariance test: if measuring the constraint''s extraction via the expansive reading yields a different ε than via the state-centric reading, they are distinct constraints. The engine will treat each reading as a separate constraint story with its own ε.',
    'If distinct, each reading gets its own classification; if not, the framework must model a single constraint with observer-dependent classification (which the ε-invariance principle forbids).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system framing under-determination: whether the kernel supports one constraint or multiple.').

omega_variable(
    coordination_extraction_boundary,
    'Is the humanitarian coordination function (protection floor) structurally separable from the extraction of compliance costs, or are they inextricably linked?',
    'Analyze whether states could provide the same protections voluntarily without the legal compulsion; examine historical compliance in conflicts where the reading is not accepted.',
    'If separable, the extraction component could be reduced without losing coordination; if linked, the extraction is the price of coordination and the constraint is inherently a tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the constraint''s coordination and extraction components are structurally separable.').

omega_variable(
    suppression_of_alternative_interpretations,
    'Does the constraint''s enforcement suppress alternative interpretations (state-centric, customary) through structural legal hierarchy, or through internalized norm acceptance?',
    'Track whether states that reject the expansive reading still comply in practice due to internalized norms, or only when compelled by courts.',
    'If suppression is internalized, the constraint''s effective suppression is higher than structural measures suggest; if structural, suppression is contingent on institutional enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_of_alternative_interpretations, conceptual, 'Structural vs. internalized suppression mechanism for competing legal interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comm_tr_t15, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(comm_tr_t30, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(comm_tr_t45, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 45, 0.25).
narrative_ontology:measurement(comm_tr_t60, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(comm_tr_t75, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 75, 0.3).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comm_be_t15, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(comm_be_t30, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(comm_be_t45, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(comm_be_t60, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(comm_be_t75, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 75, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comm_su_t15, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 15, 0.3).
narrative_ontology:measurement(comm_su_t30, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(comm_su_t45, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 45, 0.45).
narrative_ontology:measurement(comm_su_t60, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 60, 0.48).
narrative_ontology:measurement(comm_su_t75, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 75, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the common_article_3_scope kernel. The expansive reading claims universal application; the state-centric reading claims threshold-limited application; the ICRC customary reading claims practice-determined scope. Their ε values differ: expansive (0.6), state-centric (lower extraction, higher suppression of expansive claims), customary (moderate). They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
