% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Commons Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the GPL's reciprocity obligation as an
 *   institutional technology designed to prevent the enclosure of the
 *   software commons. It is a reading of the 'gpl_reciprocity_obligation'
 *   kernel, specifically the 'copyleft_as_commons_reading'. The constraint
 *   coordinates the collective action of developers to build a shared
 *   resource, but it does so by extracting a 'cost' (the obligation to share)
 *   from those who would prefer to privatize their contributions. This makes
 *   it a Tangled Rope from the perspective of this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.6).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.7).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Copyleft as Commons Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '5eb973a4-dd9f-4959-81e9-fd3e56b9383d').
narrative_ontology:cs_kernel_codification('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', fixed_text).
narrative_ontology:cs_authority_grounding('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', lineage).
narrative_ontology:cs_interpretation_layer_present('5eb973a4-dd9f-4959-81e9-fd3e56b9383d').
narrative_ontology:cs_reading_relation('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', foundational, collective_code_must_remain_open).
narrative_ontology:cs_axiom_status(collective_code_must_remain_open, holdable).
narrative_ontology:cs_axiom_grounding('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', collective_code_must_remain_open, deontological).
narrative_ontology:cs_axiom('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', secondary, proprietary_enclosure_is_harmful_to_commons).
narrative_ontology:cs_axiom_status(proprietary_enclosure_is_harmful_to_commons, holdable).
narrative_ontology:cs_axiom_grounding('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', proprietary_enclosure_is_harmful_to_commons, empirically_contingent).
narrative_ontology:cs_reference_frame('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', self_sustaining_software_commons).
narrative_ontology:cs_drift_state('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', contemporary_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5eb973a4-dd9f-4959-81e9-fd3e56b9383d', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_foundations).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizing_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, users_of_gpl_software).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continuous contribution of modifications and derivative works back into the public domain, preventing private appropriation of collective effort. The commons itself is an institutional construct that accrues value from this constraint.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, the_software_commons, beneficiary,
    institutional, generational, analytical, global).

% Administer and defend the GPL, providing legal enforcement and advocacy. They ensure compliance and promote the copyleft philosophy, acting as stewards of the software commons. Their existence is tied to the persistence of the copyleft model.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_foundations, agenda_setter,
    organized, generational, constrained, global).

% Bear the cost of mandatory reciprocity: if they use GPL-licensed code, they must release their modifications under a compatible license, preventing them from enclosing the value in proprietary products. Their exit options are to avoid GPL code or to comply, which constrains their business models.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Desire to use open-source code as a base for proprietary products without contributing back. The GPL prevents this, forcing them to choose between full reciprocity or avoiding GPL components, which can limit their technical choices and market opportunities.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, exit_maximizing_developers, payer,
    moderate, biographical, constrained, global).

% Benefit from the assurance that the software they use will remain open and modifiable, with source code always available. They are protected from vendor lock-in and can inspect, modify, and distribute the software freely.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, users_of_gpl_software, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective action problem by ensuring that contributions to a shared software commons are reciprocated, preventing free-riding and enclosure by proprietary interests. It creates a self-sustaining ecosystem of shared code.
% TRANSFER_FUNCTION: Transfers the obligation to share modifications and derivative works from individual developers/companies back to the software commons, ensuring that the collective pool of knowledge and code grows rather than being privatized.
% ABSENT_VOICES: Developers and businesses who believe in maximal freedom to choose licensing terms, including proprietary ones, are effectively excluded from using GPL code in certain ways. They would argue for less restrictive licenses that allow proprietary integration without reciprocity.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, a significant portion of the open-source software ecosystem would likely be enclosed by proprietary interests. Companies would fork open-source projects, add proprietary modifications, and sell them without contributing back, leading to a fragmentation and eventual diminishment of the software commons.
% FOUNDING_PROBLEM: The problem of software enclosure: early software development saw code being privatized, preventing users from understanding, modifying, or sharing it, leading to a loss of collective knowledge and control.
% FOUNDING_PROBLEM_CORROBORATION: Open-source advocates and legal scholars outside of specific foundations corroborate that the threat of enclosure remains live, citing ongoing attempts by proprietary entities to leverage open-source code without contributing back. The continuous legal defense of the GPL by foundations further attests to the ongoing nature of the problem.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.6) because the 'cost' of reciprocity is substantial for proprietary integrators, but it is framed as a necessary contribution to the commons rather than pure rent. Suppression is high (0.7) because the constraint's viral nature and legal enforcement actively suppress alternative business models that rely on proprietary enclosure of GPL-derived works. Theater ratio is low (0.1) as the constraint's function is largely direct and effective in achieving its stated goal of commons protection. The metrics reflect the ongoing tension between the coordination function (building the commons) and the extractive aspect (forcing reciprocity).
 *
 * PERSPECTIVAL GAP:
 *   The 'copyleft_as_commons_reading' emphasizes the collective benefit and the necessity of the reciprocity obligation, while other readings (e.g., 'copyleft_as_restriction_reading') would highlight the constraint on individual freedom or business models. The engine's classification will reflect this reading's specific structural claims.
 *
 * DIRECTIONALITY LOGIC:
 *   The software commons and open-source foundations are clear beneficiaries (d near 0.0) as they directly gain from the enforced reciprocity. Proprietary integrators and exit-maximizing developers are targets (d near 1.0) as they bear the direct cost of the reciprocity obligation. Users of GPL software are beneficiaries (d near 0.0) as they gain from the availability of open, modifiable software.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commons_enclosure_threat_level,
    'What is the actual, contemporary threat level of commons enclosure in software, and how much of the GPL''s enforcement is still addressing a live problem versus maintaining an ideological stance?',
    'Empirical studies on software forks, proprietary derivatives of open-source projects, and the economic impact of different licensing models over time.',
    'If the threat is low, the constraint''s extractiveness might be re-evaluated as higher relative to its coordination function, potentially shifting its classification towards a Snare. If the threat is high, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_enclosure_threat_level, empirical, 'Assesses whether the founding problem of enclosure is still as severe as claimed.').

omega_variable(
    reciprocity_as_extraction_vs_contribution,
    'Is the mandatory reciprocity of the GPL primarily an ''extraction'' from individual developers/companies, or a ''contribution'' to a collective good?',
    'Conceptual analysis of property rights, collective goods theory, and the philosophy of open source, alongside surveys of developer sentiment and economic impact studies.',
    'If framed primarily as extraction, the extractiveness metric might be considered higher, pushing towards a Snare. If framed as a necessary contribution, the Tangled Rope classification is more robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocity_as_extraction_vs_contribution, conceptual, 'Examines the normative framing of the reciprocity obligation.').

omega_variable(
    gpl_as_freedom_vs_commons,
    'Does the GPL primarily serve to maximize individual user freedom (as in the ''copyleft_as_freedom_reading'') or to protect the collective software commons (as in this ''copyleft_as_commons_reading'')?',
    'Analysis of legal precedents, FSF statements, and community discourse to identify the dominant normative grounding and practical effects.',
    'If the freedom aspect is dominant, the constraint might be re-evaluated under that framing, potentially altering its beneficiary/victim structure and extractiveness. If the commons aspect is dominant, this reading''s classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_as_freedom_vs_commons, conceptual, 'Distinguishes between the primary normative goals of the GPL across different readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1989, 0.05).
narrative_ontology:measurement(gpl__tr_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 1999, 0.08).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2019, 0.12).
narrative_ontology:measurement(gpl__tr_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1989, 0.5).
narrative_ontology:measurement(gpl__be_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 1999, 0.55).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2009, 0.6).
narrative_ontology:measurement(gpl__be_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(gpl__be_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1989, 0.6).
narrative_ontology:measurement(gpl__su_t1999, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 1999, 0.65).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2009, 0.7).
narrative_ontology:measurement(gpl__su_t2019, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2019, 0.72).
narrative_ontology:measurement(gpl__su_t2024, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, open_source_ecosystem_governance).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_software_development).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel, focusing on its role in preventing commons enclosure. The other readings ('copyleft_as_freedom_reading', 'copyleft_as_restriction_reading') offer different structural interpretations of the same underlying license.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
