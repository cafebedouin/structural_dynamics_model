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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   This constraint represents the 'procedural integrity' reading of the 1951
 *   Refugee Convention and its 1967 Protocol. This reading emphasizes that
 *   while states retain some flexibility in defining protection thresholds,
 *   the integrity of the individualized assessment process is non-negotiable.
 *   The outcome of a claim is secondary to ensuring a fair, transparent, and
 *   accessible procedure. This reading aims to coordinate states around a
 *   common standard of due process in asylum determination, preventing
 *   arbitrary rejections and upholding the rule of law in migration
 *   governance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.35).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.25).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__procedural_integrity_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__procedural_integrity_reading, "Refugee Convention: Procedural Integrity Reading").
narrative_ontology:topic_domain(refugee_convention_text__procedural_integrity_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__procedural_integrity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, 'b4cd5b12-8c21-4479-b0af-8a3fc1537301').
narrative_ontology:cs_kernel_codification('b4cd5b12-8c21-4479-b0af-8a3fc1537301', fixed_text).
narrative_ontology:cs_authority_grounding('b4cd5b12-8c21-4479-b0af-8a3fc1537301', lineage).
narrative_ontology:cs_interpretation_layer_present('b4cd5b12-8c21-4479-b0af-8a3fc1537301').
narrative_ontology:cs_reading_relation('b4cd5b12-8c21-4479-b0af-8a3fc1537301', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('b4cd5b12-8c21-4479-b0af-8a3fc1537301', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('b4cd5b12-8c21-4479-b0af-8a3fc1537301', foundational, individualized_assessment_is_non_negotiable).
narrative_ontology:cs_axiom_status(individualized_assessment_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('b4cd5b12-8c21-4479-b0af-8a3fc1537301', individualized_assessment_is_non_negotiable, deontological).
narrative_ontology:cs_axiom('b4cd5b12-8c21-4479-b0af-8a3fc1537301', foundational, procedural_fairness_is_paramount).
narrative_ontology:cs_axiom_status(procedural_fairness_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('b4cd5b12-8c21-4479-b0af-8a3fc1537301', procedural_fairness_is_paramount, conventional).
narrative_ontology:cs_reference_frame('b4cd5b12-8c21-4479-b0af-8a3fc1537301', post_wwii_humanitarian_consensus).
narrative_ontology:cs_drift_state('b4cd5b12-8c21-4479-b0af-8a3fc1537301', contemporary_migration_crises_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b4cd5b12-8c21-4479-b0af-8a3fc1537301', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_parties_to_convention).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, refugee_advocacy_groups).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, refugee_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, states_seeking_unilateral_exclusion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, refugee_claimants).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, border_enforcement_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking asylum who must navigate complex legal processes, bear the burden of proof, and face significant uncertainty. They benefit from the existence of a fair process, even if the outcome is not guaranteed.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, refugee_claimants, payer,
    powerless, immediate, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, refugee_claimants, beneficiary).

% States that have ratified the Refugee Convention and are bound by its provisions. They are responsible for implementing fair individualized assessment procedures. They benefit from a legitimate, shared framework for managing refugee flows and upholding international law, but bear the administrative costs.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_parties_to_convention, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__procedural_integrity_reading, states_parties_to_convention, beneficiary).

% Non-governmental organizations and legal aid groups that monitor state compliance with procedural integrity, provide legal assistance to claimants, and advocate for adherence to the Convention's principles. They benefit from a clear procedural standard to uphold.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, refugee_advocacy_groups, observer,
    organized, generational, mobile, global).

% Entities like the UNHCR, the European Court of Human Rights, and national supreme courts that interpret the Convention's procedural requirements and adjudicate disputes, ensuring consistency and integrity across jurisdictions.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_courts_and_bodies, agenda_setter,
    institutional, civilizational, analytical, universal).

% States that prioritize sovereign control over borders and seek to limit their obligations under the Convention, often by implementing policies that circumvent or weaken procedural safeguards. They bear the cost of international scrutiny and legal challenges when they deviate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_seeking_unilateral_exclusion, payer,
    powerful, biographical, constrained, national).

% Government agencies responsible for border control and initial processing of asylum claims. They are on the front lines of implementing procedural requirements and often face pressure to expedite or restrict access, creating tension with the procedural integrity mandate.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, border_enforcement_agencies, payer,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__procedural_integrity_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__procedural_integrity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common international framework and minimum procedural standards for states to assess asylum claims, ensuring a degree of fairness, legitimacy, and predictability in a complex international issue, preventing a race to the bottom in protection standards.
% TRANSFER_FUNCTION: Transfers the burden of individualized assessment from arbitrary state discretion to a standardized, albeit flexible, procedural framework. It transfers the cost of maintaining this framework to states, and the burden of proof to claimants, in exchange for a legitimate process.
% ABSENT_VOICES: Claimants denied access to fair process, or those whose claims are rejected due to procedural shortcuts or insufficient guarantees in offshore processing. They would argue for stricter adherence to procedural guarantees and greater transparency.
% DISAPPEARANCE_RATIONALE: If the procedural integrity reading of the Convention vanished overnight, states would likely revert to more arbitrary, purely sovereign-discretionary, or summary approaches to asylum claims. This would lead to chaotic and potentially inhumane outcomes for asylum seekers, undermine international legal order, and erode trust in multilateral institutions.
% FOUNDING_PROBLEM: The post-WWII displacement crisis highlighted the urgent need for an international legal framework to protect individuals fleeing persecution, ensuring states had a common, legitimate, and fair process for assessing claims and preventing refoulement.
% FOUNDING_PROBLEM_CORROBORATION: The UN High Commissioner for Refugees (UNHCR) consistently reports on ongoing mass displacement and persecution, affirming the continued relevance of the Convention's core problem. International human rights organizations and legal scholars also corroborate the persistent need for robust procedural safeguards in migration governance.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness (0.35) is moderate, reflecting the inherent administrative costs and burdens placed on both states and claimants by a complex legal process, but not designed for extraction. Suppression (0.25) is low because the constraint itself is intended to prevent arbitrary exclusion and ensure access to process, though states may still attempt to suppress access. Theater ratio (0.15) is low, as this reading prioritizes genuine procedural adherence over mere performance. The slight upward trend in extractiveness and theater reflects the increasing pressure on states to manage large caseloads, sometimes leading to more burdensome processes or performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states committed to this reading, the Convention is a vital Rope for international cooperation and rule of law. From the perspective of states seeking unilateral exclusion, it is a Snare imposing unwanted obligations. From the perspective of some advocacy groups, it is a Rope that is constantly under threat of being degraded into a Snare by state practices. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   States parties are beneficiaries of a legitimate international framework but also payers of its administrative costs. Refugee claimants are beneficiaries of a fair process but payers of the burden of proof and uncertainty. Advocacy groups and international bodies are beneficiaries of a clear standard to uphold. States seeking unilateral exclusion are victims, as the constraint imposes obligations they wish to avoid. Border enforcement agencies are payers, tasked with implementing complex procedures.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_flexibility_vs_erosion,
    'Is the ''flexible protection threshold'' inherent to this reading a necessary adaptation to state capacity and evolving circumstances, or does it create a loophole for states to erode substantive protection while maintaining a veneer of procedural compliance?',
    'Empirical analysis of state practices over time: if ''flexibility'' consistently correlates with lower protection rates without demonstrable changes in persecution patterns, it suggests erosion. Legal analysis of jurisprudence on ''margin of appreciation'' in refugee law.',
    'If it''s a loophole, the effective extractiveness of the constraint is higher than measured, as the process becomes a tool for denial. If it''s genuine adaptation, the measured extractiveness is accurate for a functional Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_flexibility_vs_erosion, empirical, 'Ambiguity between necessary procedural flexibility and substantive protection erosion.').

omega_variable(
    offshore_processing_integrity,
    'Does offshore processing, even when states claim full procedural guarantees, inherently undermine the spirit of fair individualized assessment due to practical barriers (e.g., access to legal counsel, language barriers, isolation, lack of independent oversight)?',
    'Independent monitoring reports and legal challenges from human rights bodies and courts, assessing the practical implementation of procedural safeguards in offshore contexts.',
    'If offshore processing inherently compromises integrity, the constraint''s effective suppression and extractiveness are higher than measured, as the process becomes a de facto barrier to protection. If integrity can be maintained, the current metrics are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_integrity, empirical, 'Whether offshore processing can genuinely uphold procedural integrity.').

omega_variable(
    kernel_reading_identity,
    'This constraint is one reading of the Refugee Convention text. Is this ''procedural integrity'' reading a coherent and defensible interpretation, or is it an attempt to reconcile conflicting state interests with humanitarian ideals?',
    'Conceptual analysis of international legal scholarship and jurisprudence, assessing the internal consistency and historical grounding of this reading compared to its siblings.',
    'If incoherent, the constraint''s stability as a Rope is compromised, and it may be reclassified as a Tangled Rope or Snare, reflecting the underlying tensions it attempts to mask. If coherent, its classification as a Rope is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the procedural integrity reading of the Refugee Convention text, distinct from restrictive sovereignty and expansive humanitarian readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 1951, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t1951, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1951, 0.05).
narrative_ontology:measurement(refu_tr_t1970, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(refu_tr_t1990, refugee_convention_text__procedural_integrity_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(refu_tr_t2010, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2010, 0.13).
narrative_ontology:measurement(refu_tr_t2024, refugee_convention_text__procedural_integrity_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(refu_be_t1951, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1951, 0.2).
narrative_ontology:measurement(refu_be_t1970, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(refu_be_t1990, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(refu_be_t2010, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(refu_be_t2024, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t1951, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1951, 0.15).
narrative_ontology:measurement(refu_su_t1970, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1970, 0.18).
narrative_ontology:measurement(refu_su_t1990, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(refu_su_t2010, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2010, 0.23).
narrative_ontology:measurement(refu_su_t2024, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 2024, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
