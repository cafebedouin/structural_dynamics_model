% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Universal Protective Floor Reading
 *   domain: international_humanitarian_law/legal_theory
 *
 * SUMMARY:
 *   This constraint instantiates the universal_rights_reading of the
 *   contested kernel geneva_conventions_protective_scope. The reading
 *   interprets Common Article 3 of the Geneva Conventions, supplemented by
 *   human rights law, as creating a universal protective floor applicable to
 *   all persons affected by armed conflict regardless of combatant status. It
 *   directly contests the state_centric_reading (which limits protections to
 *   Article 4 combatants) and coexists uneasily with the
 *   hybrid_proportionality_reading (which scales protections by conflict
 *   type). By expanding the beneficiary set to include all conflict-affected
 *   persons, the reading raises extractive pressure on state military
 *   operational flexibility in targeting, detention, and interrogation.
 *
 * KEY AGENTS:
 *   - state_military_operators (payer): institutional power, constrained exit â bears operational restrictions and legal exposure
 *   - civilian_populations (beneficiary): powerless, trapped exit â gains protection regardless of conflict type
 *   - non_state_actors (beneficiary): moderate power, constrained exit â gains protected status denied by state-centric reading
 *   - international_judicial_bodies (agenda_setter): institutional power, analytical exit â enforces universal floor through jurisprudence
 *   - human_rights_monitoring_bodies (observer): institutional power, analytical exit â monitors and interprets compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.74).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Universal Protective Floor Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, 'c773e579-0fe8-4419-aa77-c09d41a233b5').
narrative_ontology:cs_kernel_codification('c773e579-0fe8-4419-aa77-c09d41a233b5', formalized).
narrative_ontology:cs_authority_grounding('c773e579-0fe8-4419-aa77-c09d41a233b5', lineage).
narrative_ontology:cs_interpretation_layer_present('c773e579-0fe8-4419-aa77-c09d41a233b5').
narrative_ontology:cs_reading_relation('c773e579-0fe8-4419-aa77-c09d41a233b5', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('c773e579-0fe8-4419-aa77-c09d41a233b5', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('c773e579-0fe8-4419-aa77-c09d41a233b5', foundational, inherent_dignity_universal_protection_floor).
narrative_ontology:cs_axiom_status(inherent_dignity_universal_protection_floor, holdable).
narrative_ontology:cs_axiom_grounding('c773e579-0fe8-4419-aa77-c09d41a233b5', inherent_dignity_universal_protection_floor, deontological).
narrative_ontology:cs_axiom('c773e579-0fe8-4419-aa77-c09d41a233b5', secondary, combatant_status_irrelevant_to_minimum_protection).
narrative_ontology:cs_axiom_status(combatant_status_irrelevant_to_minimum_protection, holdable).
narrative_ontology:cs_axiom_grounding('c773e579-0fe8-4419-aa77-c09d41a233b5', combatant_status_irrelevant_to_minimum_protection, conventional).
narrative_ontology:cs_reference_frame('c773e579-0fe8-4419-aa77-c09d41a233b5', universal_humanitarian_protection_floor).
narrative_ontology:cs_drift_state('c773e579-0fe8-4419-aa77-c09d41a233b5', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c773e579-0fe8-4419-aa77-c09d41a233b5', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_actors).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operators).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, common_article_three_customary_status).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, human_rights_law_applicability_in_armed_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons in conflict zones who gain protective status under the universal reading regardless of whether the conflict is international or non-international; they receive legal guarantees against torture, arbitrary detention, and indiscriminate attack but cannot voluntarily exit the conflict zone or the legal framework's application.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, global).

% Armed groups and other non-state parties to conflict who benefit from the universal floor's prohibition on torture and inhumane treatment if captured or detained; they gain protected-person status that the state-centric reading would deny them, though they remain subject to Common Article 3 obligations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_actors, beneficiary,
    moderate, immediate, constrained, global).

% State armed forces and their command chains who bear the operational burden of the universal floor; targeting, detention, and interrogation practices are restricted regardless of adversary status, creating legal exposure for military and political leaders.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operators, payer,
    institutional, immediate, constrained, global).

% International and hybrid criminal tribunals, as well as national courts exercising universal jurisdiction, that adjudicate violations of international humanitarian law; their cumulative jurisprudence establishes and enforces the universal protective floor by interpreting Common Article 3 and human rights law as applicable to all conflict participants.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_judicial_bodies, agenda_setter,
    institutional, generational, analytical, global).

% UN treaty bodies and special procedures that monitor state compliance with human rights law during armed conflict; they produce interpretive guidance reinforcing the universal floor but lack direct enforcement power beyond naming and shaming.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single minimum standard of humane treatment for every person affected by armed conflict, preventing states and non-state actors from denying legal protection based on combatant status or conflict classification.
% TRANSFER_FUNCTION: Transfers legal protection and standing to civilian populations and non-state actors while transferring operational constraints and legal liability exposure to state military operators.
% ABSENT_VOICES: State military legal advisors advocating for unfettered operational flexibility; counterinsurgency strategists who argue that status-based targeting and interrogation limitations undermine security outcomes; populations in states that reject the universal reading's applicability to their conduct.
% DISAPPEARANCE_RATIONALE: If the universal protective floor vanished overnight, states would expand targeting authority to persons currently protected, detention standards would erode to the level of national security prerogative, and non-state actors would lose the slender legal protections they currently possess; the architecture of armed conflict regulation would fragment into status-based tiers.
% FOUNDING_PROBLEM: The absence of legal protection for persons in armed conflict who did not qualify as formal combatants under earlier treaty regimes, leading to atrocities against civilians, resistance fighters, and irregular forces with impunity.
% FOUNDING_PROBLEM_CORROBORATION: International Committee of the Red Cross and independent human rights organizations attest from outside the beneficiary seats that minimum protections remain necessary and are inconsistently applied; major military powers contest that the founding problem is solved or that the universal floor is the appropriate solution.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.74, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) is high because the reading removes the state prerogative to classify adversaries as outside legal protection, directly constraining targeting, detention, and interrogation. Suppression (0.70) is high because states actively resist the reading through interpretive narrowing, reservations, covert operations, and persistent objector claims, yet the enforcement machinery (ICC, universal jurisdiction, tribunal jurisprudence) actively suppresses these alternatives. Theater ratio (0.42) reflects moderate-high performative compliance: states frequently assert adherence to humanitarian law while simultaneously designating detainees as 'unlawful enemy combatants' or conducting black-site detention to evade the universal floor. Accessibility collapse (0.65) is substantial because once the universal reading is accepted in a legal forum, status-based exclusion arguments collapse as illegitimate. Resistance (0.78) is high due to continuous state pushback, especially from major military powers.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (civilian populations, non-state actors) experience the constraint as protective legal architecture that reduces their exposure to arbitrary violence. The payer seat (state military operators) experiences the identical legal structure as asymmetric extraction of operational discretion and increased criminal liability. The agenda-setter seat (international judicial bodies) experiences it as a necessary coordination mechanism to prevent atrocities. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations and non-state actors are structural beneficiaries (low directionality): the constraint subsidizes their security and legal standing. State military operators are structural targets (high directionality): the constraint extracts operational flexibility and imposes legal costs. International judicial bodies sit near symmetric: they gain institutional authority from the reading's expansion but bear no direct operational cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both a genuine coordination function (preventing atrocities against the vulnerable) and asymmetric extraction (restriction of state military prerogative). Without the beneficiary side, the constraint would read as a pure snare on state operations; without the payer side, it would read as a rope of universal benevolence. The coexistence of both elements, maintained by active enforcement, makes tangled_rope the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_floor_customary_status,
    'Does the universal rights reading represent binding customary international law, or is it an aspirational interpretation contested by major military powers?',
    'State practice and opinio juris analysis; examination of reservations, interpretive declarations, and persistent objector claims by major powers.',
    'If merely aspirational, epsilon is lower than measured and the constraint is closer to a scaffold or rope; if firmly customary, the high extraction and suppression measures are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_floor_customary_status, conceptual, 'Whether the universal floor is binding custom or aspirational interpretation.').

omega_variable(
    state_compliance_internalization,
    'Is state adherence to the universal floor driven by internalized legal obligation or by external enforcement pressure and reputational costs?',
    'Behavioral analysis of state conduct when enforcement probability is low (covert operations, classified detention).',
    'If purely external, the constraint is a tangled rope with high suppression; if internalized, it moves toward rope with lower active enforcement requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_internalization, empirical, 'Internalized obligation versus external coercion in state compliance.').

omega_variable(
    targeting_versus_treatment_scope,
    'Does the universal reading''s floor restrict targeting decisions directly, or only the conditions of detention and treatment post-capture?',
    'Jurisprudential tracing of whether international tribunals treat the universal floor as prohibiting certain targeting categories entirely or merely regulating conduct after capture.',
    'If the floor only regulates treatment post-capture, epsilon on targeting operations is lower than authored; if it restricts targeting itself, epsilon is accurate as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(targeting_versus_treatment_scope, conceptual, 'Scope of universal floor relative to targeting versus treatment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 25, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% The geneva_conventions_protective_scope kernel decomposes into three structurally distinct constraints (readings) because the natural-language label 'Geneva protections' conflates a state-centric status threshold, a conflict-type-scaled proportionality regime, and a universal status-irrelevant floor. Each reading carries a different epsilon, victim set, and beneficiary structure. They are linked as a constraint family rather than treated as one observable-dependent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
