% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__functional_protection_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Common Article 3 Universal Floor — Functional Protection Reading
 *   domain: international_law/armed_conflict
 *
 * SUMMARY:
 *   This constraint story instantiates the functional_protection_reading of
 *   the combatant_status_definition kernel. It asserts that Common Article 3
 *   of the Geneva Conventions establishes a universal floor of humane
 *   treatment and fair trial rights for ALL detained persons in armed
 *   conflict, irrespective of combatant status determination. The reading
 *   removes status determination as a precondition for the CA3 minimum — the
 *   protections attach to the fact of detention itself. This is a
 *   low-extraction constraint (ε ≈ 0.08) because it imposes a baseline that
 *   costs detaining authorities minimal marginal compliance burden while
 *   protecting all detainees from the most severe abuses. The constraint is
 *   claimed as Mountain because the CA3 floor has achieved customary
 *   international law status and is treated as non-derogable; it emerges from
 *   the structural logic of humanity in war rather than from state consent
 *   alone.
 *
 * KEY AGENTS:
 *   - all_detained_persons: Primary beneficiary (powerless/trapped) — receives universal floor of protections regardless of status
 *   - detaining_authorities: Agenda setter and secondary beneficiary (institutional/identity_locked) — administers detention, gains legal clarity from bright-line floor, but constrained by non-derogable obligations
 *   - state_armed_forces: Beneficiary (powerful/constrained) — their personnel receive reciprocal protections when detained
 *   - non_state_armed_groups: Beneficiary (organized/constrained) — their members receive CA3 protections without needing Article 4 status
 *   - international_courts_tribunals: Observer (analytical/analytical) — adjudicates compliance, develops jurisprudence on the floor's scope
 *   - icrc: Observer (institutional/analytical) — monitors detention conditions, promotes compliance through confidential dialogue
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.08).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.12).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, mountain).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Common Article 3 Universal Floor — Functional Protection Reading").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_law/armed_conflict").

domain_priors:emerges_naturally(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '4f090438-7081-4ea5-8cad-9a53b280f97d').
narrative_ontology:cs_kernel_codification('4f090438-7081-4ea5-8cad-9a53b280f97d', formalized).
narrative_ontology:cs_authority_grounding('4f090438-7081-4ea5-8cad-9a53b280f97d', lineage).
narrative_ontology:cs_interpretation_layer_present('4f090438-7081-4ea5-8cad-9a53b280f97d').
narrative_ontology:cs_reading_relation('4f090438-7081-4ea5-8cad-9a53b280f97d', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f090438-7081-4ea5-8cad-9a53b280f97d', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('4f090438-7081-4ea5-8cad-9a53b280f97d', foundational, ca3_universal_floor_status_independent).
narrative_ontology:cs_axiom_status(ca3_universal_floor_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('4f090438-7081-4ea5-8cad-9a53b280f97d', ca3_universal_floor_status_independent, conventional).
narrative_ontology:cs_axiom('4f090438-7081-4ea5-8cad-9a53b280f97d', secondary, humane_treatment_as_ius_cogens).
narrative_ontology:cs_axiom_status(humane_treatment_as_ius_cogens, holdable).
narrative_ontology:cs_axiom_grounding('4f090438-7081-4ea5-8cad-9a53b280f97d', humane_treatment_as_ius_cogens, deontological).
narrative_ontology:cs_reference_frame('4f090438-7081-4ea5-8cad-9a53b280f97d', geneva_1949_ca3_adoption).
narrative_ontology:cs_drift_state('4f090438-7081-4ea5-8cad-9a53b280f97d', contemporary_customary_law_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f090438-7081-4ea5-8cad-9a53b280f97d', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, detaining_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, state_armed_forces).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, human_dignity_in_armed_conflict).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, ius_cogens_prohibition_torture).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, fair_trial_as_customary_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Any person deprived of liberty in connection with an armed conflict receives the CA3 minimum protections — humane treatment, prohibition of torture and cruel treatment, prohibition of outrages on personal dignity, prohibition of summary execution, fair trial guarantees — without any requirement to prove combatant status. They are physically detained and cannot exit the constraint's scope; the constraint is the only legal structure protecting them from the most severe abuses.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% State or non-state authorities who detain persons in armed conflict. They administer the detention system and gain legal clarity from the bright-line CA3 floor (no status determination needed for baseline compliance). Their own personnel receive reciprocal protections when detained by adversaries. They are identity-locked to their role as detainers — they cannot exit the constraint without ceasing to be a detaining authority in armed conflict. They bear the marginal cost of refraining from prohibited acts, which is minimal.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_authorities, agenda_setter,
    institutional, biographical, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, detaining_authorities, beneficiary).

% Formal state military personnel. They benefit from the CA3 floor when detained by any adversary (state or non-state). They also benefit from the higher POW protections under Article 4 when the adversary recognizes their status. Their exit is constrained — they cannot individually opt out of the protections, and their state's participation in the Geneva system is a strategic commitment.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, state_armed_forces, beneficiary,
    powerful, biographical, constrained, global).

% Organized non-state armed groups party to non-international armed conflicts. Their members receive CA3 protections when detained, regardless of whether the group meets Article 4 criteria or AP I Art 1(4) criteria. This is the key structural difference from the state_centric_reading (which would deny them any protections) and the national_liberation_reading (which would grant them POW protections only if fighting colonial/occupation/racist regimes). Their exit is constrained — they are bound by the conflict's dynamics and cannot individually exit the constraint's protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_state_armed_groups, beneficiary,
    organized, biographical, constrained, global).

% Judicial bodies (ICC, ICTY, ICTR, ICJ, regional courts) that adjudicate violations of Common Article 3. They develop the jurisprudence defining the floor's scope (e.g., what constitutes 'humane treatment,' 'fair trial' in non-international conflicts). They neither collect nor pay; they interpret and enforce. Their analytical exit is absolute.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_courts_tribunals, observer,
    analytical, generational, analytical, universal).

% The International Committee of the Red Cross monitors detention conditions worldwide through confidential dialogue with detaining authorities. It promotes compliance with the CA3 floor without public condemnation. It is an institutional observer with a unique mandate; its analytical exit is structural (it could withdraw from specific contexts but not from its mandate).
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(combatant_status_definition__functional_protection_reading, diffuse).
narrative_ontology:fixing_cost_class(combatant_status_definition__functional_protection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of protection gaps in non-international armed conflicts and status-determination disputes by establishing a universal, status-independent floor of humane treatment and fair trial rights for all detained persons. The coordination is among all parties to armed conflicts: each accepts the floor for enemy detainees in exchange for the floor protecting their own personnel.
% TRANSFER_FUNCTION: Moves the burden of status determination off the baseline protections. The constraint transfers zero resources; it transfers legal risk from detainees (who no longer must prove status to claim basic protections) to detaining authorities (who must comply without status verification). The 'gain' is the universalization of the floor — no seat captures extraction.
% ABSENT_VOICES: Detained persons in active conflict zones who cannot access courts or monitoring bodies — their voices are absent not by design but by circumstance. Also absent: detaining authorities who deny the constraint's applicability (e.g., states claiming 'unlawful combatants' fall outside CA3) — they are not excluded from the conversation but reject its premises.
% DISAPPEARANCE_RATIONALE: If the CA3 universal floor vanished overnight, detainees in non-international armed conflicts and status-disputed contexts would lose all treaty-based minimum protections. Detaining authorities would face no legal baseline for treatment. The legal architecture of detention in armed conflict would collapse to raw power. The world would rearrange violently — this is not a natural fact but a constructed norm whose disappearance would be catastrophic.
% FOUNDING_PROBLEM: In 1949, the Geneva Conventions regulated international armed conflicts comprehensively but left non-international armed conflicts (civil wars) largely unregulated. The founding problem was the total absence of minimum humanitarian standards for detainees in civil wars, where status determination was impossible and the full POW framework inapplicable. Common Article 3 was the 'mini-convention' designed to fill this gap.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC (institutional observer, not a beneficiary) attests the problem is live: contemporary conflicts (Syria, Yemen, Ukraine, Sahel, Myanmar) generate mass detention in non-international and status-disputed contexts where the CA3 floor is the only applicable standard. International tribunals (ICTY, ICC) corroborate through jurisprudence: the Tadić judgment (1995) confirmed CA3 as customary law applicable to all armed conflicts. No state or armed group credibly claims the problem is solved; the constraint's expansion to transnational counterterrorism detention (Hamdan v. Rumsfeld, 2006) confirms its continuing relevance.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, ExtMetricName, E),
    domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(combatant_status_definition__functional_protection_reading),
    narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.08) because the CA3 floor imposes minimal affirmative obligations — it primarily prohibits certain acts (torture, cruel treatment, outrages on personal dignity, summary execution) rather than requiring resource-intensive positive measures. Suppression is low (0.12) because the constraint operates through legal obligation and reputational cost, not active coercion of detainees; detainees cannot 'exit' detention but the constraint does not suppress their alternatives — it creates them. Theater ratio is near zero (0.05) because compliance is substantially real where it occurs; the gap between law and practice is an enforcement deficit, not performative compliance. Accessibility collapse is very high (0.92) because once the CA3 floor is recognized as customary law, no legal alternative exists that permits lower treatment — the constraint collapses the option space for detaining authorities. Resistance is low (0.08) because the core prohibitions (torture, summary execution) face no serious normative contestation; resistance appears only at the margins (e.g., scope of 'fair trial' in non-international conflicts).
 *
 * PERSPECTIVAL GAP:
 *   The detaining_authorities seat experiences this as a coordination constraint (legal clarity, reciprocal protection for own personnel) with mild extractive friction (compliance monitoring). The all_detained_persons seat experiences it as a Mountain — the protections are non-derogable, status-independent, and survive even if the detaining authority denies their applicability. The engine computes this divergence from the structural data: detaining_authorities have institutional power and constrained exit (identity_locked to their role as lawful detainers), while all_detained_persons are powerless and trapped. The analytical observer seat sees the full structure: a genuine Mountain with a potential False Summit ambiguity (omega: natural_law_vs_constructed_ambiguity).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: all_detained_persons (primary — receive protections without preconditions) and detaining_authorities (secondary — gain legal certainty and reciprocal protections for own forces). No victims declared because the constraint imposes a floor, not a ceiling; it does not extract from detaining authorities beyond the marginal cost of refraining from prohibited acts. The directionality derivation chain assigns d ≈ 0.05 to all_detained_persons (full beneficiary, trapped), d ≈ 0.25 to detaining_authorities (agenda_setter with identity_locked exit — they administer the constraint but are bound by it), d ≈ 0.15 to state_armed_forces and non_state_armed_groups (beneficiaries with constrained exit). No override needed — the derivation matches structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1949): establishing a minimum standard of humanity in non-international armed conflicts where the full Geneva framework did not apply. Status: live — the problem persists because armed conflicts continue to generate detention outside the Article 4 framework, and new detention contexts (counterterrorism, transnational conflicts) revive the need for a status-independent floor. The mandate has not atrophied; if anything, the constraint's relevance has expanded. No mandatrophy resolution needed — the constraint remains functionally alive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Does this constraint instantiate the functional_protection_reading of the combatant_status_definition kernel, or does it describe a distinct constraint?',
    'Structural comparison: if the ε and beneficiary/victim profile matches low extraction for all detainees with status-independent protections, this is the functional_protection_reading. If status determination remains a precondition for any protection tier, it is a different constraint.',
    'Confirms this JSON is the correct reading instantiation. Misidentification would route committer content to the wrong constraint file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Reading identity confirmation for the combatant_status_definition kernel').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the universal floor of Common Article 3 protections a genuine natural law (Mountain) or a constructed norm that benefits identifiable actors (False Summit candidate)?',
    'Track whether detaining authorities who invoke the floor to limit their obligations (e.g., claiming ''we only owe CA3'') also benefit from the status ambiguity that lets them deny higher protections. If the floor functions as a ceiling, the constraint may be a false summit.',
    'If the floor operates as a ceiling in practice, FSM would reclassify to tangled_rope via the false_summit_mountain signature. This omega documents the ambiguity for the engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the CA3 universal floor is a genuine Mountain or a False Summit masking extraction').

omega_variable(
    sibling_reading_foreclosure,
    'Does the functional_protection_reading''s core premise (status-independent CA3 floor) logically foreclose the state_centric_reading (status-dependent POW protections) within a single legal framework?',
    'Legal doctrine analysis: can a single framework hold both ''all detainees get CA3 regardless of status'' and ''only Article 4 combatants get POW protections''? The former is a floor; the latter is a ceiling. They occupy different protection tiers and can coexist structurally.',
    'If they coexist, the relation is ''coexists_with'' not ''forecloses''. This determines cs_structure.reading_relations for the kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Structural relationship between functional_protection_reading and state_centric_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2016).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.03).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t1977, combatant_status_definition__functional_protection_reading, theater_ratio, 1977, 0.04).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t1995, combatant_status_definition__functional_protection_reading, theater_ratio, 1995, 0.05).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t2006, combatant_status_definition__functional_protection_reading, theater_ratio, 2006, 0.05).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t2016, combatant_status_definition__functional_protection_reading, theater_ratio, 2016, 0.05).

% Extraction over time
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.12).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t1977, combatant_status_definition__functional_protection_reading, base_extractiveness, 1977, 0.09).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t1995, combatant_status_definition__functional_protection_reading, base_extractiveness, 1995, 0.07).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t2006, combatant_status_definition__functional_protection_reading, base_extractiveness, 2006, 0.08).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t2016, combatant_status_definition__functional_protection_reading, base_extractiveness, 2016, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.15).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t1977, combatant_status_definition__functional_protection_reading, suppression_requirement, 1977, 0.12).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t1995, combatant_status_definition__functional_protection_reading, suppression_requirement, 1995, 0.1).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t2006, combatant_status_definition__functional_protection_reading, suppression_requirement, 2006, 0.12).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t2016, combatant_status_definition__functional_protection_reading, suppression_requirement, 2016, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.08).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, geneva_conventions_common_article_3).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, additional_protocol_i_article_75).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, additional_protocol_ii_article_6).

% DUAL FORMULATION NOTE:
% This reading (functional_protection) and the state_centric_reading form a constraint family linked by the combatant_status_definition kernel. The functional reading establishes the universal floor (CA3); the state_centric reading defines the ceiling (POW protections under Article 4). They are not competing measurements of the same constraint — they are structurally distinct constraints at different protection tiers. The national_liberation_reading sits between them: it expands the ceiling downward to certain non-state actors while the functional reading secures the floor for all. The ε values differ substantially: functional_protection ε ≈ 0.08 (Mountain), state_centric ε ≈ 0.35 (Tangled Rope — coordinates interstate war but extracts from non-state actors), national_liberation ε ≈ 0.25 (Rope/Tangled Rope boundary — coordinates anti-colonial struggle but conditions protection on political criteria).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
