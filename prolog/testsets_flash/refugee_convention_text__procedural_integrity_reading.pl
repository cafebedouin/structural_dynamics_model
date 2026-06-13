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
 *   This constraint represents the 'procedural integrity' reading of the 1951
 *   Refugee Convention, where the Convention's primary function is to ensure
 *   fair and individualized assessment processes for asylum seekers. Under
 *   this reading, the protection threshold itself can be flexible, but the
 *   integrity of the procedure is non-negotiable. The outcome of an asylum
 *   claim is secondary to the fairness of the process. This reading allows
 *   for practices like offshore processing, provided full procedural
 *   guarantees are in place, and permits states to narrow definitions of
 *   'well-founded fear' as long as substantive review is not eliminated.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.3).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.4).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention: Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'cb305df2-2b41-4218-9e54-7246bd629aeb').
narrative_ontology:cs_kernel_codification('cb305df2-2b41-4218-9e54-7246bd629aeb', fixed_text).
narrative_ontology:cs_authority_grounding('cb305df2-2b41-4218-9e54-7246bd629aeb', lineage).
narrative_ontology:cs_interpretation_layer_present('cb305df2-2b41-4218-9e54-7246bd629aeb').
narrative_ontology:cs_reading_relation('cb305df2-2b41-4218-9e54-7246bd629aeb', refugee_convention_text__restrictive_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('cb305df2-2b41-4218-9e54-7246bd629aeb', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('cb305df2-2b41-4218-9e54-7246bd629aeb', foundational, procedural_fairness_is_paramount).
narrative_ontology:cs_axiom_status(procedural_fairness_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('cb305df2-2b41-4218-9e54-7246bd629aeb', procedural_fairness_is_paramount, deontological).
narrative_ontology:cs_axiom('cb305df2-2b41-4218-9e54-7246bd629aeb', foundational, outcome_flexibility_within_process).
narrative_ontology:cs_axiom_status(outcome_flexibility_within_process, holdable).
narrative_ontology:cs_axiom_grounding('cb305df2-2b41-4218-9e54-7246bd629aeb', outcome_flexibility_within_process, conventional).
narrative_ontology:cs_reference_frame('cb305df2-2b41-4218-9e54-7246bd629aeb', post_wwii_procedural_justice).
narrative_ontology:cs_drift_state('cb305df2-2b41-4218-9e54-7246bd629aeb', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('cb305df2-2b41-4218-9e54-7246bd629aeb', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_seeking_orderly_migration).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, refugee_applicants_with_access).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, refugee_applicants_denied_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States interpret the Convention to allow flexibility in protection thresholds, provided fair and individualized assessment procedures are maintained. They benefit from a framework that allows for managed migration while upholding a baseline of due process.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_seeking_orderly_migration, agenda_setter,
    institutional, generational, constrained, global).

% Individuals who gain access to the procedural safeguards outlined by this reading benefit from a fair, individualized assessment of their claim, even if the outcome is not guaranteed. Their ability to present their case is protected.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, refugee_applicants_with_access, beneficiary,
    powerless, immediate, constrained, global).

% Individuals who are denied access to fair procedural assessment, for example through offshore processing without full guarantees, bear the cost of this reading's limitations. Their claims may not be heard, regardless of merit.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, refugee_applicants_denied_access, payer,
    powerless, immediate, trapped, global).

% Monitor state compliance with procedural integrity, advocating for robust and fair assessment mechanisms. They provide expert opinions and legal guidance on the interpretation of the Convention's procedural requirements.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_human_rights_bodies, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates states' efforts to manage refugee flows by establishing a common, minimum standard for fair and individualized assessment procedures, ensuring a predictable process for determining refugee status.
% TRANSFER_FUNCTION: Transfers the burden of proof and the risk of erroneous decisions from states to individual applicants when procedural access is denied or compromised, while transferring the responsibility for fair process to states when access is granted.
% ABSENT_VOICES: Refugee advocates and human rights organizations who argue for a more expansive interpretation of protection thresholds, and for universal access to fair procedures without geographical or jurisdictional limitations, are often marginalized in discussions focused solely on procedural integrity.
% DISAPPEARANCE_RATIONALE: If the procedural integrity requirement vanished, states would likely revert to arbitrary or summary assessments, leading to chaotic and unjust outcomes for asylum seekers, and undermining the international framework for refugee protection.
% FOUNDING_PROBLEM: The need for a standardized, fair, and non-discriminatory process for determining refugee status, preventing arbitrary refoulement, and ensuring states could manage migration flows with a degree of predictability.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and the UN High Commissioner for Refugees consistently corroborate the ongoing need for robust procedural safeguards in refugee determination, citing persistent challenges in state practices and the increasing complexity of forced displacement.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).

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
 *   The extractiveness (0.3) is moderate, reflecting the costs borne by applicants who, despite fair process, may still be denied protection, and by those denied access to the process itself. Suppression (0.4) is also moderate, as states actively enforce procedural requirements and may suppress alternative routes to protection. The theater ratio (0.2) is low, indicating that the commitment to procedural fairness is largely genuine, though some performative elements may exist around the edges of access. The metrics reflect a constraint that genuinely coordinates state action around a procedural standard, but with inherent costs for those navigating the system.
 *
 * PERSPECTIVAL GAP:
 *   States often view this reading as a balanced approach to migration governance, upholding international law while preserving sovereign control. Refugee advocates, however, may see it as a minimalist interpretation that prioritizes state interests over the humanitarian imperative, especially when procedural flexibility leads to de facto denial of protection.
 *
 * DIRECTIONALITY LOGIC:
 *   States seeking orderly migration are beneficiaries (agenda_setter) as this reading provides a manageable framework. Refugee applicants with access are also beneficiaries, as they receive a fair hearing. However, refugee applicants denied access to these procedures are victims, bearing the full cost of exclusion. International human rights bodies act as observers, monitoring compliance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_protection,
    'Does a focus on procedural integrity inevitably lead to a weakening of substantive protection, or can robust procedures genuinely safeguard rights even with flexible thresholds?',
    'Empirical study of outcomes in jurisdictions with strong procedural integrity but flexible thresholds, compared to those with more expansive substantive interpretations.',
    'If procedural focus consistently correlates with weaker substantive protection, this reading''s extractiveness would be re-evaluated upwards, potentially shifting its classification towards a Tangled Rope or Snare for applicants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_protection, empirical, 'The relationship between procedural integrity and substantive protection outcomes.').

omega_variable(
    offshore_processing_procedural_guarantees,
    'Can ''full procedural guarantees'' genuinely be maintained in offshore processing contexts, or does the very nature of offshore processing inherently compromise procedural integrity?',
    'Independent, transparent audits of offshore processing centers, including access to legal counsel, appeal mechanisms, and independent oversight.',
    'If full procedural guarantees are found to be systematically impossible in offshore contexts, this reading would be re-evaluated as more extractive and suppressive, as it would be enabling a de facto denial of rights under the guise of process.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(offshore_processing_procedural_guarantees, empirical, 'Feasibility of maintaining procedural integrity in offshore processing.').

omega_variable(
    reading_framing_underdetermination,
    'Is the ''procedural integrity'' framing the most defensible interpretation of the Refugee Convention, or is it a strategic framing by states to manage migration flows while appearing compliant?',
    'Analysis of state negotiating positions during the Convention''s drafting, and comparison with subsequent state practice and judicial interpretations over time.',
    'If found to be primarily a strategic framing, the ''extraction'' component of the constraint would be re-evaluated upwards, and the ''claimed_type'' might shift from Rope to Tangled Rope, reflecting a more instrumental use of the Convention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the procedural integrity reading is a genuine interpretation or a strategic framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.2).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2010, 0.32).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.3).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
