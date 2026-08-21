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
 *   Refugee Convention, emphasizing that states must provide fair,
 *   individualized assessment processes for asylum claims, even if the
 *   substantive protection threshold remains flexible. The outcome of a claim
 *   is secondary to the integrity of the procedure itself. This reading
 *   allows for offshore processing or stricter definitions of 'well-founded
 *   fear,' provided that full procedural guarantees are maintained and
 *   substantive review is not eliminated. It is one reading of the
 *   'refugee_convention_text' kernel, distinct from more expansive
 *   humanitarian or restrictive sovereignty interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__procedural_integrity_reading, 0.4).
domain_priors:suppression_score(refugee_convention_text__procedural_integrity_reading, 0.6).
domain_priors:theater_ratio(refugee_convention_text__procedural_integrity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__procedural_integrity_reading, extractiveness, 0.4).
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
narrative_ontology:cs_story_uid(refugee_convention_text__procedural_integrity_reading, '7418562b-6541-49b3-b8f3-90ac38d220f8').
narrative_ontology:cs_kernel_codification('7418562b-6541-49b3-b8f3-90ac38d220f8', fixed_text).
narrative_ontology:cs_authority_grounding('7418562b-6541-49b3-b8f3-90ac38d220f8', lineage).
narrative_ontology:cs_interpretation_layer_present('7418562b-6541-49b3-b8f3-90ac38d220f8').
narrative_ontology:cs_reading_relation('7418562b-6541-49b3-b8f3-90ac38d220f8', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('7418562b-6541-49b3-b8f3-90ac38d220f8', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_axiom('7418562b-6541-49b3-b8f3-90ac38d220f8', foundational, due_process_is_non_negotiable).
narrative_ontology:cs_axiom_status(due_process_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('7418562b-6541-49b3-b8f3-90ac38d220f8', due_process_is_non_negotiable, deontological).
narrative_ontology:cs_axiom('7418562b-6541-49b3-b8f3-90ac38d220f8', foundational, state_sovereignty_is_procedurally_bounded).
narrative_ontology:cs_axiom_status(state_sovereignty_is_procedurally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('7418562b-6541-49b3-b8f3-90ac38d220f8', state_sovereignty_is_procedurally_bounded, conventional).
narrative_ontology:cs_reference_frame('7418562b-6541-49b3-b8f3-90ac38d220f8', post_wwii_legal_order_procedural_minimum).
narrative_ontology:cs_drift_state('7418562b-6541-49b3-b8f3-90ac38d220f8', contemporary_migration_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7418562b-6541-49b3-b8f3-90ac38d220f8', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__procedural_integrity_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, states_seeking_orderly_migration).
narrative_ontology:constraint_beneficiary(refugee_convention_text__procedural_integrity_reading, international_legal_institutions).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, asylum_seekers_without_access_to_fair_process).
narrative_ontology:constraint_victim(refugee_convention_text__procedural_integrity_reading, human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States benefit from a framework that allows them to manage migration flows while adhering to international law, provided the procedural requirements are met. They seek to maintain sovereign control over borders while fulfilling their non-refoulement obligations through fair process.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, states_seeking_orderly_migration, agenda_setter,
    institutional, generational, constrained, national).

% These institutions benefit from the Convention's continued adherence, as it underpins a significant portion of international human rights law. Their legitimacy is tied to the integrity of the international legal order, which this reading emphasizes.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, international_legal_institutions, beneficiary,
    institutional, civilizational, analytical, global).

% These individuals bear the cost of procedural failures, facing refoulement or prolonged detention without a substantive review of their claims. Their access to protection is entirely dependent on the integrity of the assessment process.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, asylum_seekers_without_access_to_fair_process, payer,
    powerless, immediate, trapped, regional).

% These groups expend significant resources monitoring state compliance with procedural safeguards and litigating on behalf of asylum seekers. They are victims when procedural integrity is compromised, as it undermines the very mechanism they advocate for.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, human_rights_advocates, payer,
    organized, biographical, constrained, global).

% These actors would argue for maximum state discretion, potentially viewing any procedural requirement as an infringement on sovereignty. While not directly paying, their policy preferences are constrained by this reading's emphasis on process.
narrative_ontology:constraint_stakeholder(refugee_convention_text__procedural_integrity_reading, restrictive_sovereignty_proponents, excluded,
    powerful, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common international standard for assessing refugee claims, ensuring that states, while maintaining sovereign control, adhere to a minimum threshold of fair and individualized procedural assessment before making protection decisions.
% TRANSFER_FUNCTION: Transfers the burden of proof and the risk of procedural error onto asylum seekers when processes are inadequate, while transferring legitimacy and order to states that implement fair procedures.
% ABSENT_VOICES: Asylum seekers who are denied access to any process, or whose claims are summarily dismissed without individualized assessment, are structurally excluded. They would argue for substantive protection over mere procedural adherence.
% DISAPPEARANCE_RATIONALE: If the procedural integrity requirement of the Convention vanished, states would likely revert to purely discretionary or summary processes, leading to widespread refoulement and a collapse of the international protection regime, forcing a global reorganization of migration governance.
% FOUNDING_PROBLEM: The post-WWII need for an international framework to prevent the refoulement of individuals fleeing persecution, ensuring that states provide a minimum standard of assessment for those seeking asylum.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, human rights organizations, and the UNHCR consistently corroborate the ongoing need for procedural safeguards to prevent arbitrary refoulement, even as states contest the scope of substantive protection.
narrative_ontology:disappearance_verdict(refugee_convention_text__procedural_integrity_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__procedural_integrity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__procedural_integrity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(refugee_convention_text__procedural_integrity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__procedural_integrity_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness (0.4) is moderate because while the process itself is a benefit, failures in that process can lead to severe outcomes for asylum seekers. Suppression (0.6) is significant because states actively enforce their procedural requirements, and asylum seekers have limited recourse if these are denied. Theater ratio (0.2) is low, as the procedural requirements are generally taken seriously, though there's always a risk of performative compliance masking substantive denial. The metrics reflect a system that genuinely attempts to coordinate fair process but can still be extractive for those caught in its failures.
 *
 * PERSPECTIVAL GAP:
 *   States prioritize the orderly management of migration and the legitimacy conferred by adhering to international law, seeing the procedural integrity as a coordination mechanism. Asylum seekers and advocates, however, experience the constraint as potentially extractive if procedural failures lead to unjust outcomes, highlighting the gap between process and substantive protection.
 *
 * DIRECTIONALITY LOGIC:
 *   States and international legal institutions are beneficiaries, as this reading provides a legitimate framework for managing migration and upholding international law. Asylum seekers without access to fair process and human rights advocates are victims, bearing the costs of procedural failures and the effort to ensure compliance. Proponents of restrictive sovereignty are 'excluded' in this reading, as their desire for unfettered discretion is constrained by the non-negotiable procedural demands.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    procedural_vs_substantive_protection,
    'To what extent can procedural integrity genuinely substitute for a high substantive protection threshold, or does a low substantive threshold inevitably undermine procedural fairness?',
    'Empirical studies comparing outcomes for asylum seekers under regimes with high procedural integrity but varying substantive thresholds. If procedural integrity consistently fails to prevent refoulement under low substantive thresholds, the substitution is illusory.',
    'If procedural integrity cannot substitute for substantive protection, this reading''s classification as a Tangled Rope would shift closer to Snare, as its coordination function (fair process) would be revealed as cover for extraction (denial of protection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_vs_substantive_protection, empirical, 'The effectiveness of procedural safeguards in the absence of robust substantive protection.').

omega_variable(
    offshore_processing_procedural_guarantees,
    'Can offshore processing arrangements genuinely provide the full procedural guarantees required by this reading, or do they inherently compromise fair individualized assessment?',
    'Independent monitoring and legal review of specific offshore processing regimes, assessing access to legal counsel, independent review, and non-refoulement safeguards. If no offshore regime can meet the standard, the practice is incompatible with this reading.',
    'If offshore processing is found to be inherently incompatible with full procedural guarantees, this reading would either be forced to reject such practices or its own internal coherence would be compromised, potentially shifting its classification towards a Snare due to the suppression of effective process.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(offshore_processing_procedural_guarantees, empirical, 'Compatibility of offshore processing with procedural integrity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__procedural_integrity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__procedural_integrity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(refu_tr_t6, refugee_convention_text__procedural_integrity_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(refu_tr_t12, refugee_convention_text__procedural_integrity_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement(refu_tr_t18, refugee_convention_text__procedural_integrity_reading, theater_ratio, 18, 0.17).
narrative_ontology:measurement(refu_tr_t24, refugee_convention_text__procedural_integrity_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__procedural_integrity_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(refu_be_t6, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(refu_be_t12, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 12, 0.35).
narrative_ontology:measurement(refu_be_t18, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 18, 0.37).
narrative_ontology:measurement(refu_be_t24, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__procedural_integrity_reading, base_extractiveness, 30, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(refu_su_t6, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(refu_su_t12, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(refu_su_t18, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(refu_su_t24, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 24, 0.59).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__procedural_integrity_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
