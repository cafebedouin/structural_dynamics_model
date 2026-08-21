% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Refugee Convention: Restrictive Sovereignty Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint represents a restrictive interpretation of the 1951
 *   Refugee Convention, emphasizing state sovereignty and limiting protection
 *   obligations. It defines 'well-founded fear' as requiring individualized
 *   persecution proof and restricts 'particular social group' to immutable
 *   characteristics with state awareness. This reading permits high
 *   admissibility screening and offshore processing, effectively narrowing
 *   the victim set and excluding those fleeing generalized violence or
 *   non-state persecution. The constraint is claimed as a 'rope' by its
 *   proponents (a necessary coordination mechanism for states) but operates
 *   with significant extraction and suppression, making it a 'tangled_rope'
 *   in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.75).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Refugee Convention: Restrictive Sovereignty Reading").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'ee099623-8731-4a16-a893-e4f7e3457cb9').
narrative_ontology:cs_kernel_codification('ee099623-8731-4a16-a893-e4f7e3457cb9', fixed_text).
narrative_ontology:cs_authority_grounding('ee099623-8731-4a16-a893-e4f7e3457cb9', lineage).
narrative_ontology:cs_interpretation_layer_present('ee099623-8731-4a16-a893-e4f7e3457cb9').
narrative_ontology:cs_reading_relation('ee099623-8731-4a16-a893-e4f7e3457cb9', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('ee099623-8731-4a16-a893-e4f7e3457cb9', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('ee099623-8731-4a16-a893-e4f7e3457cb9', foundational, sovereign_discretion_primacy).
narrative_ontology:cs_axiom_status(sovereign_discretion_primacy, holdable).
narrative_ontology:cs_axiom_grounding('ee099623-8731-4a16-a893-e4f7e3457cb9', sovereign_discretion_primacy, conventional).
narrative_ontology:cs_axiom('ee099623-8731-4a16-a893-e4f7e3457cb9', foundational, individualized_persecution_proof_standard).
narrative_ontology:cs_axiom_status(individualized_persecution_proof_standard, holdable).
narrative_ontology:cs_axiom_grounding('ee099623-8731-4a16-a893-e4f7e3457cb9', individualized_persecution_proof_standard, conventional).
narrative_ontology:cs_reference_frame('ee099623-8731-4a16-a893-e4f7e3457cb9', westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('ee099623-8731-4a16-a893-e4f7e3457cb9', contemporary_migration_crises, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ee099623-8731-4a16-a893-e4f7e3457cb9', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Convention to maximize national discretion over borders and minimize protection obligations, viewing it as a minimum floor rather than an expansive mandate. They benefit from reduced intake and control over who enters.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, constrained, national).

% Implement policies derived from the restrictive reading, focusing on admissibility screening, individualized proof of persecution, and limiting 'particular social group' definitions. Their mandate is to enforce state sovereignty and control migration flows.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, border_control_agencies, agenda_setter,
    organized, biographical, constrained, national).

% Face high barriers to protection, requiring individualized proof of persecution often difficult to obtain, and exclusion from protection if their fear stems from generalized violence or non-state actors. Their options are limited to navigating complex legal systems or returning to danger.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Work to assist asylum seekers and challenge restrictive interpretations through legal and public advocacy. They bear the costs of litigation and public education against a powerful institutional agenda.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, refugee_advocacy_groups, payer,
    moderate, biographical, constrained, global).

% Monitor state compliance with international law, including the Refugee Convention. They issue reports and recommendations but lack direct enforcement power over sovereign states, observing the impact of restrictive interpretations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common, albeit minimal, framework for states to manage refugee flows and prevent refoulement, ensuring some level of international cooperation on a shared challenge.
% TRANSFER_FUNCTION: Transfers the burden of proof for persecution onto individual asylum seekers, and the discretion to grant protection to sovereign states, while limiting the scope of state obligations.
% ABSENT_VOICES: Refugees and displaced persons themselves, whose lived experiences of generalized violence and non-state persecution are often excluded from the narrow definitions of 'well-founded fear' and 'particular social group' under this reading.
% DISAPPEARANCE_RATIONALE: If this restrictive reading vanished, states would face immediate pressure to adopt more expansive interpretations, potentially leading to increased asylum grants, changes in border processing, and a significant shift in the balance between state sovereignty and humanitarian obligations.
% FOUNDING_PROBLEM: The need for an international legal framework to protect individuals fleeing persecution, particularly in the aftermath of World War II, balancing state sovereignty with humanitarian concerns.
% FOUNDING_PROBLEM_CORROBORATION: Sovereign states and border control agencies attest the problem is live, emphasizing ongoing security and migration management challenges. Refugee advocacy groups and international human rights bodies corroborate the problem's existence but contest the restrictive interpretation as an appropriate solution, arguing it undermines the Convention's original intent.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.68) is high due to the significant burden placed on asylum seekers to prove individualized persecution, often in contexts where such proof is difficult to obtain. Suppression (0.75) is also high, as states actively enforce narrow interpretations through legal and administrative barriers, including detention and expedited removal processes. The theater ratio (0.20) is relatively low, indicating that while there is some performative adherence to humanitarian principles, the primary function of this reading is to control borders and limit state obligations. The metrics reflect a system that, while claiming to coordinate, primarily extracts from vulnerable populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of sovereign states, this reading is a necessary 'rope' for managing national security and economic stability, ensuring orderly migration. From the perspective of asylum seekers and their advocates, it functions as a 'snare' or 'tangled_rope', trapping individuals in precarious situations and extracting their right to protection through narrow legal definitions and procedural hurdles. The engine's classification as 'tangled_rope' reflects this divergence, acknowledging a coordination function (for states) alongside significant extraction (from asylum seekers).
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and border control agencies are clear beneficiaries and agenda-setters, as this reading grants them maximum discretion and control over migration, reducing the perceived 'burden' of refugees. Asylum seekers are the primary victims, facing high hurdles to protection. Refugee advocacy groups, while not direct victims of persecution, bear the costs of challenging this restrictive framework. International human rights bodies act as observers, documenting the impact without direct enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by highlighting how a framework initially designed for coordination (the Convention itself) can be interpreted to become highly extractive. The 'live' status of the founding problem (protecting individuals fleeing persecution) contrasts with the restrictive interpretation, suggesting a drift where the mechanism designed to solve the problem now actively limits its solution for many. The high extractiveness and suppression, coupled with the 'contested' status of the founding problem, indicate that the constraint's operation has shifted significantly from its original humanitarian mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_persecution_definition,
    'Is ''well-founded fear'' limited to individualized persecution by state actors, or does it encompass generalized violence and persecution by non-state actors?',
    'International court rulings or state practice shifts that explicitly broaden or narrow the definition of persecution beyond individual state-sponsored acts.',
    'If broadened, the victim set would expand significantly, increasing state obligations and reducing extractiveness. If strictly maintained, the current high extractiveness persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_persecution_definition, conceptual, 'Ambiguity in the definition of ''well-founded fear'' and its scope.').

omega_variable(
    particular_social_group_definition,
    'Is ''particular social group'' strictly limited to immutable characteristics with state awareness, or can it include evolving social constructs like gender, sexual orientation, or clan affiliation?',
    'Jurisprudential evolution in national and international courts, or legislative amendments to national asylum laws that explicitly expand or restrict the definition.',
    'An expansive definition would increase the number of individuals eligible for protection, reducing extractiveness. A narrow definition maintains the current restrictive framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(particular_social_group_definition, conceptual, 'Ambiguity in the definition of ''particular social group''.').

omega_variable(
    sovereignty_vs_humanitarian_balance,
    'What is the appropriate balance between state sovereignty over borders and international humanitarian obligations under the Refugee Convention?',
    'Ongoing international legal and political discourse, potentially leading to new treaties or widely accepted interpretations that re-calibrate this balance.',
    'A shift towards greater humanitarian obligation would reduce state discretion and extractiveness; a reinforcement of sovereignty would maintain or increase current levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_humanitarian_balance, preference, 'Fundamental tension between state sovereignty and humanitarian duties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1951, 0.4).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1951, 0.5).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Refugee Convention text, each with different structural properties and classifications. This restrictive reading emphasizes state sovereignty and narrow protection criteria.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
