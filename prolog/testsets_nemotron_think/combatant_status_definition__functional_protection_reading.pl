% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__functional_protection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: combatant_status_definition__functional_protection_reading
 *   human_readable: Universal Common Article 3 Protections for All Detainees
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint story captures the functional_protection_reading of the
 *   combatant_status_definition kernel: the position that Common Article 3 of
 *   the Geneva Conventions establishes a universal, status-independent floor
 *   of humane treatment and fair trial protections for all persons detained
 *   in armed conflict. The reading removes combatant status determination as
 *   a precondition for baseline protections, treating CA3 as a coordination
 *   mechanism that solves the problem of protection gaps by making the floor
 *   universal. The constraint has low extractiveness (ε=0.12) because it
 *   imposes baseline humanitarian obligations that are the legitimate cost of
 *   conducting detention in armed conflict, not predatory extraction.
 *   Suppression is low (0.18) because the rule eliminates a coercive gate
 *   (status determination) rather than creating one. Theater ratio has risen
 *   modestly (0.10→0.22) as states increasingly perform compliance through
 *   status-determination rituals while denying substantive protections in
 *   practice.
 *
 * KEY AGENTS:
 *   - detaining_powers: Primary payer (institutional/constrained) — bear compliance costs; administer detention
 *   - all_detained_persons: Primary beneficiary (powerless/trapped to identity_locked) — receive universal CA3 floor
 *   - icrc: Observer (institutional/analytical) — monitors compliance, promotes universal application
 *   - states_parties: Agenda_setter (institutional/generational) — treaty parties defining the rule
 *   - non_state_armed_groups: Payer/beneficiary dual (organized/constrained) — bound by CA3 in NIAC, benefit when their members detained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__functional_protection_reading, 0.12).
domain_priors:suppression_score(combatant_status_definition__functional_protection_reading, 0.18).
domain_priors:theater_ratio(combatant_status_definition__functional_protection_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(combatant_status_definition__functional_protection_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__functional_protection_reading, rope).
narrative_ontology:human_readable(combatant_status_definition__functional_protection_reading, "Universal Common Article 3 Protections for All Detainees").
narrative_ontology:topic_domain(combatant_status_definition__functional_protection_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__functional_protection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__functional_protection_reading, '55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8').
narrative_ontology:cs_kernel_codification('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', formalized).
narrative_ontology:cs_authority_grounding('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', lineage).
narrative_ontology:cs_interpretation_layer_present('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8').
narrative_ontology:cs_reading_relation('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', combatant_status_definition__national_liberation_reading, coexists_with).
narrative_ontology:cs_axiom('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', foundational, common_article_3_universal_application).
narrative_ontology:cs_axiom_status(common_article_3_universal_application, holdable).
narrative_ontology:cs_axiom_grounding('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', common_article_3_universal_application, conventional).
narrative_ontology:cs_axiom('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', foundational, status_determination_not_precondition_for_humane_treatment).
narrative_ontology:cs_axiom_status(status_determination_not_precondition_for_humane_treatment, holdable).
narrative_ontology:cs_axiom_grounding('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', status_determination_not_precondition_for_humane_treatment, deontological).
narrative_ontology:cs_reference_frame('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', humanitarian_protection_floor).
narrative_ontology:cs_drift_state('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', contemporary_ihl_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('55baaf6a-df4c-48b5-aaad-7bc02fe1b7a8', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__functional_protection_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, all_detained_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, detaining_powers).
narrative_ontology:constraint_victim(combatant_status_definition__functional_protection_reading, non_state_armed_groups).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, humanitarian_protection_floor).
narrative_ontology:constraint_vindicates(combatant_status_definition__functional_protection_reading, non_discrimination_in_detention).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States and organized armed groups that detain persons in armed conflict. They administer detention, bear the costs of providing humane treatment, fair trials, and prohibited acts prevention. They set detention policy and can influence status determination procedures. Exit from CA3 obligations is constrained — withdrawal from Geneva Conventions does not relieve customary obligations, and non-compliance triggers international responsibility.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, detaining_powers, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, detaining_powers, payer).

% Every person deprived of liberty in connection with an armed conflict, regardless of nationality, status, or alleged conduct. They receive the CA3 floor: humane treatment, prohibition of violence to life and person, prohibition of outrages upon personal dignity, fair trial guarantees. They cannot exit detention voluntarily; their protection depends entirely on the detaining power's compliance.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, all_detained_persons, beneficiary,
    powerless, immediate, trapped, universal).

% The International Committee of the Red Cross monitors detention conditions, visits detainees, and promotes universal CA3 application. It operates under a mandate from the Geneva Conventions and the international community. Its analytical seat allows it to see the full structure of protection gaps and compliance failures across all parties to conflict.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, icrc, observer,
    institutional, generational, analytical, global).

% States party to the Geneva Conventions and Additional Protocols. They define the treaty framework, participate in diplomatic conferences, and can influence customary law formation. They have arbitrage-grade exit: they can shape the rule's evolution through state practice and opinio juris, but cannot unilaterally escape customary obligations.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, states_parties, agenda_setter,
    institutional, generational, arbitrage, global).

% Organized non-state armed groups engaged in non-international armed conflicts. They are bound by CA3 when they detain persons (payer role: must provide humane treatment, fair trials). Their members benefit when detained by states (beneficiary role: receive CA3 floor). Exit is constrained: they cannot opt out of CA3 in NIAC, and their capacity to provide protections depends on resources and control.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(combatant_status_definition__functional_protection_reading, non_state_armed_groups, beneficiary).

% ICJ, ICC, ICTY, ICTR, and other tribunals that adjudicate CA3 violations and interpret its scope. They provide authoritative interpretation of the universal floor, but their jurisdiction is consent-based or Security Council-referred. Their analytical seat shapes the rule's evolution through jurisprudence.
narrative_ontology:constraint_stakeholder(combatant_status_definition__functional_protection_reading, international_courts_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, status-independent floor of humane treatment and fair trial protections for all persons detained in armed conflict, eliminating the protection gap created by status determination procedures.
% TRANSFER_FUNCTION: Moves the baseline cost of humane detention (food, shelter, medical care, fair trial procedures, prohibition of torture/ill-treatment) from detainees to detaining powers. The transfer is universal — every detaining power pays for every detainee — and does not vary with combatant status.
% ABSENT_VOICES: Detainees in 'black sites' and undeclared conflict zones who are denied any legal categorization; future generations affected by precedent-setting erosion of the CA3 floor; civilian populations in occupied territories whose protection depends on the floor's integrity.
% DISAPPEARANCE_RATIONALE: If the universal CA3 floor vanished overnight, states would immediately revert to status-based protection regimes. Detainees classified as 'unlawful combatants,' 'terrorists,' or 'criminals' would lose all treaty-based minimum protections. The coordination problem (who gets the floor?) would reopen with no default answer, leading to protection fragmentation and increased abuse.
% FOUNDING_PROBLEM: The 1949 Diplomatic Conference adopted Common Article 3 to address the complete absence of treaty protections in non-international armed conflicts and the risk that status determination would be used to deny protections in international conflicts. The founding problem was protection gaps created by legal categorization.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 2016 Commentary on GC III Art 3 confirms the founding problem remains live: 'The need for a minimum standard of protection in all armed conflicts, regardless of their classification, is as pressing today as it was in 1949.' UN Special Rapporteurs on counter-terrorism and human rights have documented states using status determinations to circumvent CA3. The 2023 ICRC report 'International Humanitarian Law and the Challenges of Contemporary Armed Conflicts' corroborates from outside the benefiting parties that protection gaps persist.
narrative_ontology:disappearance_verdict(combatant_status_definition__functional_protection_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__functional_protection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__functional_protection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__functional_protection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__functional_protection_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__functional_protection_reading_tests).
:- end_tests(combatant_status_definition__functional_protection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The functional reading extracts minimally because it universalizes the baseline — no detainee falls below the floor. Extraction would arise if the rule were used to deny higher protections (POW status) while claiming CA3 compliance, but the reading itself only asserts the floor. The coordination function is genuine: it solves the 'who gets minimum protections?' problem by answering 'everyone.' Suppression is low because the rule removes the status gate; the measured 0.18 reflects residual state resistance to universal application (e.g., 'unlawful combatant' categorizations). Theater ratio growth tracks the gap between formal acceptance of CA3 universality and substantive compliance in high-stakes conflicts.
 *
 * PERSPECTIVAL GAP:
 *   From the detaining power seat, the constraint appears as a cost-imposing obligation (extraction-adjacent). From the detainee seat, it appears as a life-preserving guarantee (pure coordination). From the ICRC seat, it appears as a monitoring framework. The engine computes these divergences from the structural data; the authored claim (rope) reflects the rule's design logic, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Detaining powers are payers (d→1.0): they bear the costs of humane treatment, fair trials, and prohibition of violence to life/person. Their exit is constrained — they cannot opt out of CA3 without violating IHL. All detained persons are beneficiaries (d→0.0): they receive the protection floor regardless of status; their exit is trapped/identity_locked (they cannot exit detention). ICRC and states_parties sit near analytical/symmetric (d≈0.5): they administer and monitor the rule. Non-state armed groups are dual: payers when detaining (constrained exit), beneficiaries when their members are detained (identity_locked). The universal floor compresses directionality spread compared to status-gated readings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing protection gaps through status manipulation — remains live. The constraint has not suffered mandatrophy; its function (universal floor) is still needed because states continue to use status determinations to deny protections. The reading does not persist by inertia; it persists because the coordination problem it solves (who gets the floor?) recurs in every conflict. Theater growth reflects compliance drift, not functional atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does the functional_protection_reading of the combatant_status_definition kernel structurally differ from its sibling readings in its allocation of protections and burdens?',
    'Comparative analysis of the three readings'' beneficiary/victim structures, directionality profiles, and the specific legal mechanisms each reading activates or suppresses.',
    'Clarifies whether the three readings are structurally distinct constraints (per ε-invariance) or observational variants of one constraint. Determines if the functional reading''s universal floor creates a genuinely different extraction profile than the status-gated readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing of the combatant_status_definition kernel: this reading instantiates a universal CA3 floor; state_centric_reading gates POW protections behind formal status; national_liberation_reading extends combatant status to specific non-state actors under AP I Art 1(4).').

omega_variable(
    ca3_customary_vs_treaty_scope,
    'Is the universal CA3 floor this reading establishes a customary international law obligation (binding all states) or a treaty obligation (binding only parties to GC III/AP II)?',
    'ICJ and ICTY jurisprudence on CA3 customary status; state practice and opinio juris analysis for non-party states.',
    'If customary, the constraint''s spatial_scope is universal and its suppression metric reflects near-total alternative collapse for detaining powers. If treaty-only, scope contracts to parties and suppression drops — the reading''s coordination function becomes opt-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ca3_customary_vs_treaty_scope, empirical, 'Customary vs. treaty scope of the CA3 universal floor.').

omega_variable(
    enforcement_gap_vs_rule_extraction,
    'Does the measured suppression (0.18) reflect the rule''s inherent coercive structure or the enforcement gap between the rule and state practice?',
    'Separate the rule''s textual suppression (status-determination eliminated as precondition) from compliance data. If suppression rises when measuring compliance rather than rule structure, the constraint story may conflate the rule with its violation.',
    'If suppression is primarily an enforcement-gap artifact, the rule itself may be a purer rope (lower suppression) and the extraction profile changes. Affects Boltzmann coordination purity assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_vs_rule_extraction, conceptual, 'Disentangling rule-structural suppression from compliance-failure suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__functional_protection_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t1949, combatant_status_definition__functional_protection_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t1977, combatant_status_definition__functional_protection_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t1995, combatant_status_definition__functional_protection_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t2001, combatant_status_definition__functional_protection_reading, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t2010, combatant_status_definition__functional_protection_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_tr_t2024, combatant_status_definition__functional_protection_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t1949, combatant_status_definition__functional_protection_reading, base_extractiveness, 1949, 0.08).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t1977, combatant_status_definition__functional_protection_reading, base_extractiveness, 1977, 0.1).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t1995, combatant_status_definition__functional_protection_reading, base_extractiveness, 1995, 0.11).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t2001, combatant_status_definition__functional_protection_reading, base_extractiveness, 2001, 0.12).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t2010, combatant_status_definition__functional_protection_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_be_t2024, combatant_status_definition__functional_protection_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t1949, combatant_status_definition__functional_protection_reading, suppression_requirement, 1949, 0.12).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t1977, combatant_status_definition__functional_protection_reading, suppression_requirement, 1977, 0.14).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t1995, combatant_status_definition__functional_protection_reading, suppression_requirement, 1995, 0.16).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t2001, combatant_status_definition__functional_protection_reading, suppression_requirement, 2001, 0.18).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t2010, combatant_status_definition__functional_protection_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(combatant_status_definition__functional_protection_reading_su_t2024, combatant_status_definition__functional_protection_reading, suppression_requirement, 2024, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__functional_protection_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(combatant_status_definition__functional_protection_reading, 0.1).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, combatant_status_definition__national_liberation_reading).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, pow_status_determination_procedures).
narrative_ontology:affects_constraint(combatant_status_definition__functional_protection_reading, non_international_armed_conflict_detention_regime).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the combatant_status_definition constraint family. The functional reading establishes the universal CA3 floor (low ε, rope). The state_centric reading gates POW privileges behind formal status (higher ε for non-state actors, tangled_rope). The national_liberation reading extends combatant status to specific non-state actors (moderate ε, rope for qualifying groups). They are linked because each reading's ε and beneficiary structure is defined by its position on the status-determination axis — the kernel's central structural variable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__functional_protection_reading, institutional, 0.75).
constraint_indexing:directionality_override(combatant_status_definition__functional_protection_reading, powerless, 0.05).
constraint_indexing:directionality_override(combatant_status_definition__functional_protection_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
