% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta (1215) - Universal Due Process Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models the 'universal rights' reading of Magna
 *   Carta (1215), specifically Clause 39, where 'free men' is interpreted to
 *   mean all persons, establishing a transhistorical precedent for universal
 *   due process. This reading views Magna Carta not as a feudal contract for
 *   a select few, but as a foundational document for individual liberties
 *   against arbitrary state power, evolving into a 'mountain' of
 *   constitutional principle. The low extractiveness and suppression reflect
 *   its status as a widely accepted, fundamental legal principle, though its
 *   application is continuously defended.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.15).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.05).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, mountain).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta (1215) - Universal Due Process Reading").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:emerges_naturally(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, '616cae0e-e26b-4888-b428-42d9e8a4c6d8').
narrative_ontology:cs_kernel_codification('616cae0e-e26b-4888-b428-42d9e8a4c6d8', fixed_text).
narrative_ontology:cs_authority_grounding('616cae0e-e26b-4888-b428-42d9e8a4c6d8', lineage).
narrative_ontology:cs_interpretation_layer_present('616cae0e-e26b-4888-b428-42d9e8a4c6d8').
narrative_ontology:cs_reading_relation('616cae0e-e26b-4888-b428-42d9e8a4c6d8', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('616cae0e-e26b-4888-b428-42d9e8a4c6d8', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('616cae0e-e26b-4888-b428-42d9e8a4c6d8', foundational, universal_human_dignity).
narrative_ontology:cs_axiom_status(universal_human_dignity, holdable).
narrative_ontology:cs_axiom_grounding('616cae0e-e26b-4888-b428-42d9e8a4c6d8', universal_human_dignity, deontological).
narrative_ontology:cs_axiom('616cae0e-e26b-4888-b428-42d9e8a4c6d8', foundational, state_power_subordinate_to_law).
narrative_ontology:cs_axiom_status(state_power_subordinate_to_law, holdable).
narrative_ontology:cs_axiom_grounding('616cae0e-e26b-4888-b428-42d9e8a4c6d8', state_power_subordinate_to_law, conventional).
narrative_ontology:cs_reference_frame('616cae0e-e26b-4888-b428-42d9e8a4c6d8', enlightenment_universal_rights_framework).
narrative_ontology:cs_drift_state('616cae0e-e26b-4888-b428-42d9e8a4c6d8', contemporary_human_rights_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('616cae0e-e26b-4888-b428-42d9e8a4c6d8', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, all_persons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, rule_of_law_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, state_actors).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, due_process_principle).
narrative_ontology:constraint_vindicates(magna_carta_1215__universal_rights_reading, habeas_corpus_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals are protected by the universal due process principles derived from Magna Carta, ensuring protection against arbitrary state action. Their benefit is the existence of a foundational legal check on power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, all_persons, beneficiary,
    powerless, generational, trapped, universal).

% Government officials, law enforcement, and judicial bodies are constrained by the requirement to adhere to due process, preventing arbitrary detention or punishment. They bear the cost of procedural adherence.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, state_actors, payer,
    institutional, generational, constrained, national).

% Interpret and apply Magna Carta's principles, expanding its scope to cover all persons and modern due process requirements. They actively shape the understanding and enforcement of this constraint.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, legal_scholars_and_judiciary, agenda_setter,
    institutional, civilizational, analytical, global).

% The abstract principle of the rule of law benefits from Magna Carta's enduring status as a foundational document for universal rights, reinforcing its legitimacy and historical depth.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, rule_of_law_doctrine, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(magna_carta_1215__universal_rights_reading, rule_of_law_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal standard for fair legal process, coordinating state power to ensure predictable and just treatment of individuals, preventing arbitrary rule.
% TRANSFER_FUNCTION: Transfers the burden of proof and procedural adherence from the individual to the state, ensuring that state power is exercised through established legal channels rather than arbitrary decree.
% ABSENT_VOICES: Historically, those excluded from the 'free men' definition (e.g., serfs, women, non-landowners) were absent. In this reading, their historical exclusion is overcome by the universal application of the principle.
% DISAPPEARANCE_RATIONALE: If the universal due process principles derived from Magna Carta vanished, it would fundamentally alter the legal landscape, removing a core historical and conceptual check on state power, leading to a significant increase in arbitrary detention and extrajudicial actions globally.
% FOUNDING_PROBLEM: The problem of arbitrary monarchical power and the need for a foundational legal document to constrain it, ensuring basic rights and legal procedures.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, constitutional scholars, and human rights organizations universally corroborate the ongoing relevance of constraining arbitrary power, citing contemporary abuses and the continuous need for due process protections. This corroboration comes from independent academic and advocacy bodies, not solely from state actors.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, ExtMetricName, E),
    domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(magna_carta_1215__universal_rights_reading),
    narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.05) reflect the widespread acceptance of due process as a fundamental legal principle, making it a 'mountain' in this reading. While state actors are 'payers' in terms of procedural adherence, the constraint's primary function is to protect individuals, making 'all_persons' the primary beneficiary. The 'emerges_naturally: true' is used to reflect its status as a foundational, almost axiomatic, legal principle in this interpretation, despite its historical origins. The slight increase in extractiveness over time reflects the increasing complexity and demands of modern due process, rather than a shift to an extractive mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_persons', this constraint is a pure benefit, a fundamental protection. From the perspective of 'state_actors', it is a necessary procedural cost. The 'legal_scholars_and_judiciary' act as agenda-setters, actively shaping and expanding this interpretation, ensuring its persistence and universal application.
 *
 * DIRECTIONALITY LOGIC:
 *   'All_persons' are full beneficiaries (d=0.0) as the constraint protects them from arbitrary power. 'State_actors' are targets (d=1.0) as they are constrained by due process requirements. 'Legal_scholars_and_judiciary' are agenda-setters, actively maintaining and expanding the constraint, thus benefiting from its continued relevance and their role in its interpretation (d near beneficiary end).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a foundational legal principle as a mere historical artifact or an extractive mechanism. By interpreting 'free men' universally, it ensures the constraint's mandate remains live and relevant, preventing mandatrophy by continuously adapting its scope to contemporary understandings of human rights. The 'live' status of the founding problem (constraining arbitrary power) further reinforces this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_scope_ambiguity,
    'Is Magna Carta''s Clause 39 genuinely a transhistorical precedent for universal rights, or is this a modern reinterpretation that overstates its original intent?',
    'Detailed historical-legal analysis of 13th-century English common law and feudal contracts, comparing the ''universal rights'' reading against the ''baronial privilege'' reading''s evidence.',
    'If resolved as primarily a feudal contract, the constraint''s ''emerges_naturally'' claim would be weakened, and its ''mountain'' classification would be challenged, potentially reclassifying it as a ''rope'' or ''tangled_rope'' with a more limited beneficiary set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_scope_ambiguity, empirical, 'Ambiguity regarding the original intent and scope of Magna Carta''s protections.').

omega_variable(
    interpretive_authority_legitimacy,
    'Is the expansion of ''free men'' to ''all persons'' a legitimate evolution of legal principle, or an act of judicial activism that strains the original text?',
    'Analysis of constitutional theory on originalism vs. living constitutionalism, examining the philosophical and jurisprudential justifications for evolving interpretations of foundational texts.',
    'If deemed an illegitimate strain, the ''authority_grounding'' for this reading would shift from ''lineage'' to ''practice'' or ''extraction'', and its ''mountain'' classification would be challenged, potentially leading to a ''tangled_rope'' or ''snare'' if the expansion is seen as serving specific institutional interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'The legitimacy of expanding the scope of Magna Carta''s protections beyond its original historical context.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.05).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_1215__universal_rights_reading, theater_ratio, 1688, 0.08).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_1215__universal_rights_reading, theater_ratio, 1789, 0.09).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__universal_rights_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1688, magna_carta_1215__universal_rights_reading, base_extractiveness, 1688, 0.12).
narrative_ontology:measurement(magn_be_t1789, magna_carta_1215__universal_rights_reading, base_extractiveness, 1789, 0.13).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.14).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__universal_rights_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__universal_rights_reading, suppression_requirement, 1215, 0.05).
narrative_ontology:measurement(magn_su_t1688, magna_carta_1215__universal_rights_reading, suppression_requirement, 1688, 0.05).
narrative_ontology:measurement(magn_su_t1789, magna_carta_1215__universal_rights_reading, suppression_requirement, 1789, 0.05).
narrative_ontology:measurement(magn_su_t1948, magna_carta_1215__universal_rights_reading, suppression_requirement, 1948, 0.05).
narrative_ontology:measurement(magn_su_t2024, magna_carta_1215__universal_rights_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, habeas_corpus_act_1679).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, us_bill_of_rights).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, universal_declaration_of_human_rights).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'magna_carta_1215' kernel. This 'universal_rights_reading' interprets 'free men' as all persons, establishing universal due process. The 'baronial_privilege_reading' sees it as a feudal contract for barons, and the 'living_document_reading' views it as an adaptive constitutional substrate. Each reading constitutes a distinct constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
