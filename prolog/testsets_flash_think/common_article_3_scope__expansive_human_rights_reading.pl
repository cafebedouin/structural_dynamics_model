% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: Common Article 3: Expansive Human Rights Reading
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the 'expansive human rights reading'
 *   of Common Article 3 (CA3) of the Geneva Conventions. This reading asserts
 *   that CA3's minimum humanitarian standards apply to any organized armed
 *   violence, regardless of its formal classification as an international or
 *   non-international armed conflict. It seeks to close loopholes that states
 *   and armed groups might exploit to deny humanitarian protection. The
 *   constraint is actively enforced by human rights bodies and international
 *   criminal courts, and actively resisted by states and armed groups seeking
 *   operational flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.78).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.85).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3: Expansive Human Rights Reading").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '9507a0f0-4fa1-41a7-be7d-9c298d20fc6d').
narrative_ontology:cs_kernel_codification('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', fixed_text).
narrative_ontology:cs_authority_grounding('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', lineage).
narrative_ontology:cs_interpretation_layer_present('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d').
narrative_ontology:cs_reading_relation('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', foundational, human_dignity_universal_and_non_derogable).
narrative_ontology:cs_axiom_status(human_dignity_universal_and_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', human_dignity_universal_and_non_derogable, deontological).
narrative_ontology:cs_axiom('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', foundational, conflict_classification_irrelevant_to_basic_humanity).
narrative_ontology:cs_axiom_status(conflict_classification_irrelevant_to_basic_humanity, holdable).
narrative_ontology:cs_axiom_grounding('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', conflict_classification_irrelevant_to_basic_humanity, conventional).
narrative_ontology:cs_reference_frame('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', universal_human_rights_framework).
narrative_ontology:cs_drift_state('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', contemporary_human_rights_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('9507a0f0-4fa1-41a7-be7d-9c298d20fc6d', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, affected_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_law_regime).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, states_armed_groups_seeking_flexibility).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, military_commanders_operators).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, universal_human_dignity_principle).
narrative_ontology:constraint_vindicates(common_article_3_scope__expansive_human_rights_reading, non_derogable_humanitarian_standards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The overarching legal framework and its institutions that seek to apply and enforce IHL, including CA3, as broadly as possible to protect human dignity in armed conflict.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_law_regime, agenda_setter,
    institutional, civilizational, analytical, universal).

% Organizations and individuals who actively promote and litigate for the broadest possible application of CA3, seeing it as a critical tool for accountability and protection. They benefit from its expansive interpretation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Civilians, detainees, and other non-combatants caught in situations of organized armed violence, regardless of formal conflict classification. They are the direct beneficiaries of the minimum humanitarian standards this reading seeks to apply universally.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, affected_populations, beneficiary,
    powerless, immediate, trapped, local).

% States and non-state armed groups that prefer narrower interpretations of CA3 to maintain operational flexibility, avoid external scrutiny, and limit accountability for actions in conflicts they do not formally classify as 'armed conflict' or 'NIAC'.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, states_armed_groups_seeking_flexibility, payer,
    institutional, biographical, constrained, global).

% Individuals responsible for planning and executing military or security operations. They bear the direct costs of adhering to broad humanitarian standards, which may constrain tactics, require specific treatment of detainees, and increase legal risk.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, military_commanders_operators, payer,
    powerful, biographical, constrained, national).

% Judicial bodies that prosecute individuals for war crimes and crimes against humanity. They enforce the application of CA3, often adopting expansive interpretations to ensure accountability for serious violations, thereby strengthening the constraint.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_courts, agenda_setter,
    institutional, generational, analytical, global).

% The International Committee of the Red Cross, which promotes and develops IHL. While it advocates for humanitarian principles, its customary law reading focuses on state practice and opinio juris, which can be more conservative than the expansive human rights reading.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal floor of minimum humanitarian standards for all organized armed violence, ensuring basic human dignity is protected regardless of conflict classification, thereby coordinating behavior towards humane treatment and reducing suffering.
% TRANSFER_FUNCTION: Transfers the 'right' to classify conflicts as outside IHL from states/armed groups to a universal standard of human dignity, imposing costs of adherence and accountability on those who would violate it, and granting protection to affected populations.
% ABSENT_VOICES: The voices of future victims of violence in unclassified conflicts, who would suffer if this constraint did not exist or was narrowly applied, are structurally absent from the debate but are represented by human rights advocates.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, states and armed groups would revert to narrower interpretations, leading to increased suffering, impunity, and a significant erosion of humanitarian protection in conflicts not formally classified as international or non-international armed conflicts. The legal landscape for armed violence would fundamentally shift.
% FOUNDING_PROBLEM: The historical problem of states and armed groups evading humanitarian obligations by denying the legal classification of conflicts, leading to widespread human rights abuses and impunity in situations of organized violence that did not meet traditional IHL thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Human rights reports, UN investigations, and academic studies consistently document ongoing abuses in situations where IHL application is contested due to classification ambiguities, corroborating the problem's persistence from outside the benefiting parties.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.78) reflects the cost imposed on states and armed groups who would prefer to operate outside IHL's minimum standards. The high suppression (0.85) indicates the active legal and political pressure exerted to ensure this broad application, suppressing attempts to narrow CA3's scope. The low theater ratio (0.15) reflects that the application of CA3, when it occurs, is generally genuine and not merely performative. Resistance is high (0.80) because this reading directly challenges state sovereignty and military prerogatives. Accessibility collapse is moderate-high (0.70) as it significantly limits the alternative of denying IHL application based on classification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human rights advocates and affected populations, this constraint is a vital Rope or even Mountain, ensuring fundamental protections. From the perspective of states and armed groups, it is a Snare or Tangled Rope, imposing unwanted restrictions and accountability. The engine's computation will reveal these per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The international humanitarian law regime, human rights advocates, and international criminal courts are beneficiaries and agenda-setters, pushing for and enforcing this expansive reading. Affected populations are the primary beneficiaries of the protections it offers. States and armed groups seeking flexibility, along with military commanders, are the primary targets/payers, as they bear the costs of adherence and accountability. The ICRC, while a key actor in IHL, is positioned as an observer in this specific reading due to its more conservative, customary law approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_classification_ambiguity,
    'Is the concept of ''organized armed violence'' sufficiently clear to apply CA3 universally without formal conflict classification, or does it introduce new ambiguities?',
    'Analysis of jurisprudence from international and national courts applying the ''organized armed violence'' standard, and empirical studies of its practical application in diverse conflict settings.',
    'If the standard proves consistently clear, it strengthens the expansive reading''s claim to universal applicability. If it introduces new, significant ambiguities, it may weaken the constraint''s effective scope and increase resistance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_ambiguity, empirical, 'Clarity of ''organized armed violence'' as a universal trigger for CA3.').

omega_variable(
    state_resistance_sustainability,
    'To what extent can states and armed groups sustainably resist the expansive application of CA3 in the face of growing international human rights jurisprudence and accountability mechanisms?',
    'Longitudinal study of state practice, legal challenges, and the effectiveness of international accountability mechanisms in compelling adherence to the expansive reading over time.',
    'If resistance proves unsustainable, the constraint''s effective suppression and extractiveness will increase, potentially shifting its classification towards a more firmly established Rope or even Mountain for states. If resistance remains effective, the constraint remains a contested Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_resistance_sustainability, empirical, 'Sustainability of state resistance to expansive CA3 application.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''reading'' of the common_article_3_scope kernel, or does it constitute a distinct, new legal norm that merely draws inspiration from CA3?',
    'Legal-historical analysis of the evolution of human rights law and IHL, focusing on whether the expansive interpretation maintains a continuous interpretive link to the original text and intent of CA3, or if it represents a normative departure.',
    'If it is a genuine reading, its legitimacy is grounded in the established authority of the Geneva Conventions. If it is a new norm, its authority must be established independently, potentially weakening its immediate force but allowing for greater flexibility in its development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the expansive interpretation is an interpretation of CA3 or a new norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(comm_tr_t1969, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1969, 0.15).
narrative_ontology:measurement(comm_tr_t1989, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(comm_tr_t2004, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(comm_be_t1969, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1969, 0.58).
narrative_ontology:measurement(comm_be_t1989, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1989, 0.65).
narrative_ontology:measurement(comm_be_t2004, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2004, 0.72).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(comm_su_t1969, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1969, 0.68).
narrative_ontology:measurement(comm_su_t1989, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1989, 0.75).
narrative_ontology:measurement(comm_su_t2004, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2004, 0.8).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, human_rights_treaty_application).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel, each representing a distinct structural claim about the application of minimum humanitarian standards in armed violence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
