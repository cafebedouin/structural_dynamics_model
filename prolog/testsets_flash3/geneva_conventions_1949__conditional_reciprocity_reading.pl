% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (1949) - Conditional Reciprocity Reading
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'conditional reciprocity' reading of the
 *   1949 Geneva Conventions, which holds that the full protections of
 *   international humanitarian law apply only when adversaries comply with
 *   its provisions, particularly regarding the status of combatants.
 *   Non-compliance by irregular forces is seen to permit a proportional
 *   degradation of protections. This reading is a specific interpretation of
 *   the Conventions, distinct from more absolute humanitarian or
 *   security-maximizing views. It allows for a moderate level of extraction
 *   from irregular combatants and civilians, justified by the perceived need
 *   for reciprocal enforcement and state security.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.45).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.6).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (1949) - Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '4de4be1a-b873-4e84-9217-91263476da99').
narrative_ontology:cs_kernel_codification('4de4be1a-b873-4e84-9217-91263476da99', fixed_text).
narrative_ontology:cs_authority_grounding('4de4be1a-b873-4e84-9217-91263476da99', lineage).
narrative_ontology:cs_interpretation_layer_present('4de4be1a-b873-4e84-9217-91263476da99').
narrative_ontology:cs_reading_relation('4de4be1a-b873-4e84-9217-91263476da99', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('4de4be1a-b873-4e84-9217-91263476da99', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('4de4be1a-b873-4e84-9217-91263476da99', foundational, protections_are_reciprocal).
narrative_ontology:cs_axiom_status(protections_are_reciprocal, holdable).
narrative_ontology:cs_axiom_grounding('4de4be1a-b873-4e84-9217-91263476da99', protections_are_reciprocal, conventional).
narrative_ontology:cs_axiom('4de4be1a-b873-4e84-9217-91263476da99', foundational, irregular_forces_lack_full_combatant_privilege).
narrative_ontology:cs_axiom_status(irregular_forces_lack_full_combatant_privilege, holdable).
narrative_ontology:cs_axiom_grounding('4de4be1a-b873-4e84-9217-91263476da99', irregular_forces_lack_full_combatant_privilege, conventional).
narrative_ontology:cs_reference_frame('4de4be1a-b873-4e84-9217-91263476da99', post_wwii_state_centric_ihl).
narrative_ontology:cs_drift_state('4de4be1a-b873-4e84-9217-91263476da99', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4de4be1a-b873-4e84-9217-91263476da99', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_military_forces).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_governments).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate under the interpretation that POW protections are conditional on adversary compliance with Article 4 criteria. They benefit from the flexibility to deny full protections to irregular forces, reducing operational constraints, but are also bound by reciprocal expectations for their own personnel.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_military_forces, agenda_setter,
    institutional, biographical, constrained, global).

% Benefit from a framework that allows for a robust response to irregular warfare while maintaining a veneer of international legality. They face pressure to uphold some standards to protect their own forces, but prioritize national security and operational effectiveness.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_governments, beneficiary,
    institutional, generational, constrained, global).

% Are denied full POW status if they do not meet Article 4 criteria, making them vulnerable to indefinite detention and harsher treatment. Their options are to comply with rules designed for state armies (often impossible) or face severe consequences.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Experience a narrowing of absolute immunity due to proportionality calculations, especially when irregular forces operate among them. They bear the costs of 'collateral damage' and the erosion of clear protected status.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilian_populations_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Monitor compliance and advocate for broader protections, often challenging the conditional reciprocity reading. They document violations and push for interpretations that prioritize humanitarian principles over military expediency.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_organizations, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reciprocal restraint in armed conflict, aiming to limit the brutality of war by providing incentives for parties to treat each other's combatants and civilians humanely, under the condition of mutual compliance.
% TRANSFER_FUNCTION: Transfers the burden of compliance and risk from state military forces to irregular combatants and civilian populations, by making protections conditional on specific criteria and allowing for proportional degradation in response to non-compliance.
% ABSENT_VOICES: Victims of 'collateral damage' and irregular combatants denied POW status are largely unheard in the formulation and interpretation of these rules, their perspectives marginalized by the state-centric nature of international law.
% DISAPPEARANCE_RATIONALE: If this reading of the Conventions vanished, state military forces would lose a key justification for their actions in asymmetric conflicts, potentially leading to either a more absolute humanitarian standard or a complete breakdown of any pretense of restraint, fundamentally altering the conduct of warfare.
% FOUNDING_PROBLEM: The need to regulate the conduct of warfare and protect non-combatants, particularly after the atrocities of World War II, by establishing clear rules for belligerents.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and humanitarian organizations, while often critical of this reading's implications, generally corroborate the historical problem of regulating warfare. State governments and military strategists attest to the ongoing challenge of asymmetric conflict and the need for flexible rules.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).
:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the costs borne by irregular combatants denied POW status and civilians affected by proportionality calculations. Suppression (0.6) is necessary to enforce this interpretation against challenges from humanitarian groups and to maintain the state's prerogative in asymmetric conflict. The theater ratio (0.2) indicates that while genuine efforts are made to adhere to some aspects of the Conventions, a portion of the enforcement is performative, aimed at legitimizing actions that might otherwise be seen as violations. The spike in extractiveness and suppression around 2001 reflects the 'War on Terror' era, where interpretations shifted to prioritize state security over broader humanitarian protections, before a slight moderation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state actors, this reading provides a necessary framework for managing asymmetric conflict and ensuring the security of their forces. From the perspective of irregular combatants and humanitarian advocates, it represents a significant erosion of universal protections and an increase in suffering for those caught in conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   State military forces and governments are beneficiaries, gaining operational flexibility and reduced constraints. Irregular combatants and civilian populations are victims, bearing the costs of conditional protections and increased vulnerability. International humanitarian organizations act as observers, challenging the interpretation and advocating for broader application of protections.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by highlighting the dual function: it coordinates reciprocal restraint among states (a 'rope' aspect) but simultaneously extracts from non-state actors and civilians through conditional application (a 'snare' aspect). The 'tangled_rope' classification captures this hybrid nature, where the coordination story is used to justify asymmetric extraction, rather than being a pure coordination mechanism or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calculation_ambiguity,
    'How are ''proportionality'' calculations applied in practice, and to what extent do they genuinely minimize civilian harm versus rationalize military objectives?',
    'Independent, real-time monitoring and post-conflict analysis of targeting decisions and civilian casualty rates, with access to military planning documents.',
    'If proportionality is consistently found to prioritize military advantage over civilian protection, the effective extractiveness from civilian populations is higher than currently assessed, pushing the constraint closer to a Snare for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculation_ambiguity, empirical, 'Ambiguity in the practical application of proportionality in targeting decisions.').

omega_variable(
    article_4_criteria_relevance,
    'Are the Article 4 criteria for POW status (organized command, distinctive insignia, carrying arms openly) still relevant and appropriate for contemporary asymmetric conflicts, or do they unfairly disadvantage irregular forces?',
    'International legal review and expert consensus on adapting IHL to modern conflict dynamics, potentially leading to new protocols or interpretations.',
    'If criteria are deemed outdated, the denial of POW status to irregulars becomes a more direct form of extraction, increasing the constraint''s Snare-like qualities for that group. If still deemed relevant, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_4_criteria_relevance, conceptual, 'Relevance of traditional POW criteria in modern asymmetric warfare.').

omega_variable(
    reciprocity_vs_humanitarian_imperative,
    'To what extent should the principle of reciprocity dictate the application of humanitarian law, versus an absolute humanitarian imperative?',
    'Philosophical and legal debate, potentially leading to new international conventions or a shift in state practice and judicial interpretation.',
    'A stronger emphasis on humanitarian imperative would reduce the conditional nature of protections, lowering extractiveness and suppression. A stronger emphasis on reciprocity would reinforce the current reading, potentially increasing extraction if non-compliance is widespread.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_vs_humanitarian_imperative, preference, 'The fundamental tension between reciprocity and humanitarian principles in IHL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.3).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.65).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 1949 Geneva Conventions kernel. This 'conditional reciprocity' reading emphasizes reciprocal compliance, while the 'humanitarian ceiling' reading posits absolute minimums, and the 'security maximization' reading prioritizes state security over convention adherence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
