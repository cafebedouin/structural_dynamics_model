% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__procedural_integrity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__procedural_integrity_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: refugee_convention_text__procedural_integrity_reading
 *   human_readable: Refugee Convention: Procedural Integrity Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents a 'procedural integrity' reading of the 1951
 *   Refugee Convention, where the emphasis is on fair individualized
 *   assessment processes rather than a broad, expansive definition of
 *   protection. The protection threshold is seen as flexible, allowing states
 *   discretion, but the integrity of the assessment procedure is
 *   non-negotiable. This reading acknowledges the Convention's humanitarian
 *   goals but prioritizes state sovereignty in implementation, leading to a
 *   'tangled rope' classification where states benefit from orderly migration
 *   and legal legitimacy, while asylum seekers and advocates bear the costs
 *   of navigating complex, outcome-flexible procedures. Offshore processing
 *   is permissible under this reading, provided full procedural guarantees
 *   are met.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.45).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.6).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention: Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'b8af9a61-7417-4c80-9137-b43652d8051d').
narrative_ontology:cs_kernel_codification('b8af9a61-7417-4c80-9137-b43652d8051d', fixed_text).
narrative_ontology:cs_authority_grounding('b8af9a61-7417-4c80-9137-b43652d8051d', lineage).
narrative_ontology:cs_interpretation_layer_present('b8af9a61-7417-4c80-9137-b43652d8051d').
narrative_ontology:cs_reading_relation('b8af9a61-7417-4c80-9137-b43652d8051d', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('b8af9a61-7417-4c80-9137-b43652d8051d', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('b8af9a61-7417-4c80-9137-b43652d8051d', foundational, procedural_due_process_is_paramount).
narrative_ontology:cs_axiom_status(procedural_due_process_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b8af9a61-7417-4c80-9137-b43652d8051d', procedural_due_process_is_paramount, deontological).
narrative_ontology:cs_axiom('b8af9a61-7417-4c80-9137-b43652d8051d', secondary, state_sovereignty_informs_interpretation).
narrative_ontology:cs_axiom_status(state_sovereignty_informs_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('b8af9a61-7417-4c80-9137-b43652d8051d', state_sovereignty_informs_interpretation, conventional).
narrative_ontology:cs_reference_frame('b8af9a61-7417-4c80-9137-b43652d8051d', fair_individualized_assessment_framework).
narrative_ontology:cs_drift_state('b8af9a61-7417-4c80-9137-b43652d8051d', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b8af9a61-7417-4c80-9137-b43652d8051d', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_seeking_orderly_migration).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, international_legal_framework).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_at_borders).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States interpret the Convention as primarily a procedural framework, allowing flexibility in protection thresholds as long as fair, individualized assessments are conducted. They benefit from maintaining control over borders and migration flows while adhering to international legal norms. They enforce procedural requirements and can define 'well-founded fear' within these bounds.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_seeking_orderly_migration, agenda_setter,
    institutional, generational, constrained, national).

% Individuals seeking asylum bear the direct costs of this reading, as their access to protection depends entirely on navigating complex, often restrictive, procedural hurdles. Their claims are assessed individually, but the outcome is secondary to the integrity of the process itself, which can lead to exclusion even with genuine fear if procedures are not met.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_at_borders, payer,
    powerless, immediate, trapped, regional).

% Advocacy groups work to ensure that procedural safeguards are robust and genuinely fair, often challenging state practices that undermine the spirit of individualized assessment. They bear the cost of constant vigilance and legal battles to uphold procedural integrity against state attempts to streamline or restrict access.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, human_rights_advocates, payer,
    organized, biographical, constrained, global).

% The broader international legal system benefits from this reading by maintaining the Convention's legitimacy as a functional, adaptable instrument of international law, even if its substantive protections are subject to state interpretation within procedural bounds. It provides a framework for dispute resolution and normative coherence.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_legal_framework, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(refugee_convention_text__procedural_integrity_reading, international_legal_framework).

% The UN Refugee Agency monitors state compliance with the Convention, providing guidance on interpretation and advocating for fair procedures. While not directly benefiting or paying, its mandate is to ensure the Convention's principles are upheld, often mediating between states and asylum seekers.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, unhcr, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state actions to ensure a minimum standard of procedural fairness in assessing asylum claims, preventing arbitrary refoulement while allowing states flexibility in defining protection thresholds.
% TRANSFER_FUNCTION: Transfers the burden of proof and procedural compliance onto asylum seekers, while transferring legitimacy and control over migration policy to states, mediated by international legal norms.
% ABSENT_VOICES: Asylum seekers who are denied access to fair procedures due to restrictive interpretations or practical barriers, and who lack the means to challenge these decisions, are effectively silenced. Their experiences of persecution are not heard if the procedural gate is closed.
% DISAPPEARANCE_RATIONALE: If this procedural integrity reading of the Convention vanished, states would lose a key framework for managing migration within a legal context, potentially leading to more arbitrary border policies and increased international legal disputes. Asylum seekers would face even greater uncertainty without a recognized standard for assessment, however imperfect.
% FOUNDING_PROBLEM: The post-WWII need to prevent states from arbitrarily returning refugees to persecution, establishing a legal framework for international protection and burden-sharing.
% FOUNDING_PROBLEM_CORROBORATION: States attest the problem is live, citing ongoing refugee crises and the need for orderly migration. Human rights advocates and UNHCR corroborate the continued need for protection against refoulement, though they dispute the adequacy of current procedural interpretations.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__procedural_integrity_reading_tests).
:- end_tests(refugee_convention_text__procedural_integrity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while the procedural framework offers some protection, it also allows states to narrow definitions and create hurdles, effectively extracting compliance and limiting access. Suppression (0.6) is significant as states actively enforce these procedural barriers, making alternatives to the official process difficult. Theater ratio (0.2) is low, as the procedural mechanisms are genuinely functional, though their outcomes are often contested. The temporal measurements show a gradual increase in extractiveness and suppression as states have increasingly emphasized procedural control over expansive protection.
 *
 * PERSPECTIVAL GAP:
 *   States view this reading as a balanced approach to international obligations and sovereign control, ensuring order. Asylum seekers and advocates, however, experience it as a system that prioritizes process over substantive protection, often leading to exclusion despite genuine fear. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   States (states_seeking_orderly_migration) are beneficiaries, gaining legitimacy and control over migration. Asylum seekers (asylum_seekers_at_borders) are targets, facing high costs and constrained exit options. Human rights advocates are also targets, as they expend significant resources to ensure procedural fairness. The international legal framework is a beneficiary, as the Convention's adaptability maintains its relevance.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by acknowledging the genuine coordination function of establishing a legal framework for refugee status determination, while also identifying the extractive elements arising from states' emphasis on procedural control and flexibility in protection outcomes. It avoids classifying it as a pure snare by recognizing the Convention's foundational role in preventing refoulement, but highlights the costs imposed by this specific interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_fairness_threshold,
    'What constitutes ''fair individualized assessment'' in practice, and at what point do procedural hurdles become so onerous that they effectively deny substantive protection?',
    'Empirical studies of asylum claim processing outcomes across different jurisdictions, combined with legal analysis of due process standards in international human rights law.',
    'If procedural hurdles are found to consistently deny substantive protection, the extractiveness and suppression metrics would be re-evaluated upwards, potentially reclassifying the constraint towards a snare. If procedures are genuinely fair, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_fairness_threshold, empirical, 'Ambiguity in the practical definition of ''fair individualized assessment''.').

omega_variable(
    offshore_processing_procedural_guarantees,
    'Can offshore processing genuinely provide ''full procedural guarantees'' equivalent to those available on sovereign territory, or does the very nature of offshore processing inherently compromise fairness?',
    'Independent monitoring and legal review of offshore processing centers, assessing access to legal counsel, independent review, and conditions of detention.',
    'If offshore processing is found to inherently compromise procedural guarantees, this reading''s permissibility of such practices would be challenged, leading to a re-evaluation of its alignment with the Convention''s core principles and an increase in its perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_procedural_guarantees, conceptual, 'Whether offshore processing can meet procedural integrity standards.').

omega_variable(
    state_discretion_vs_humanitarian_mandate,
    'What is the appropriate balance between state sovereign discretion in migration policy and the Convention''s humanitarian mandate to protect refugees?',
    'Ongoing international legal and political discourse, evolving state practice, and jurisprudence from international courts. This is a preference-based question with no definitive empirical resolution.',
    'A shift in international consensus towards prioritizing humanitarian mandate over state discretion would push this reading towards a more extractive classification, as its current balance would be seen as insufficient. Conversely, a shift towards greater state discretion would normalize its current extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_discretion_vs_humanitarian_mandate, preference, 'The fundamental tension between state sovereignty and humanitarian protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.3).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.4).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__procedural_integrity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__procedural_integrity_reading, refugee_convention_text__expansive_humanitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Refugee Convention text. Each reading represents a distinct structural claim about the Convention's function and impact, with different beneficiaries, victims, and extractiveness profiles. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
