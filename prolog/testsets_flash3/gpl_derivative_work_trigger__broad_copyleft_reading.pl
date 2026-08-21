% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Derivative Work Trigger (Linking)
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'broad copyleft' reading of the GPL's
 *   derivative work clause, specifically as it applies to linking. Under this
 *   reading, any software that links to GPL-licensed code (even dynamically)
 *   is considered a derivative work and must also be licensed under the GPL,
 *   requiring source disclosure. This interpretation is a core mechanism for
 *   the GPL's 'viral' nature, expanding the open-source commons. This is one
 *   reading of the 'gpl_derivative_work_trigger' kernel, alongside
 *   'narrow_linking_permissive_reading' and 'interface_boundary_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.3).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.4).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Derivative Work Trigger (Linking)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '3089ce6d-f84b-4cad-899a-e2275fd732e1').
narrative_ontology:cs_kernel_codification('3089ce6d-f84b-4cad-899a-e2275fd732e1', fixed_text).
narrative_ontology:cs_authority_grounding('3089ce6d-f84b-4cad-899a-e2275fd732e1', lineage).
narrative_ontology:cs_interpretation_layer_present('3089ce6d-f84b-4cad-899a-e2275fd732e1').
narrative_ontology:cs_reading_relation('3089ce6d-f84b-4cad-899a-e2275fd732e1', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('3089ce6d-f84b-4cad-899a-e2275fd732e1', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('3089ce6d-f84b-4cad-899a-e2275fd732e1', foundational, linking_constitutes_derivation).
narrative_ontology:cs_axiom_status(linking_constitutes_derivation, holdable).
narrative_ontology:cs_axiom_grounding('3089ce6d-f84b-4cad-899a-e2275fd732e1', linking_constitutes_derivation, conventional).
narrative_ontology:cs_axiom('3089ce6d-f84b-4cad-899a-e2275fd732e1', secondary, copyleft_expands_commons).
narrative_ontology:cs_axiom_status(copyleft_expands_commons, holdable).
narrative_ontology:cs_axiom_grounding('3089ce6d-f84b-4cad-899a-e2275fd732e1', copyleft_expands_commons, instrumental).
narrative_ontology:cs_reference_frame('3089ce6d-f84b-4cad-899a-e2275fd732e1', gpl_v2_original_intent).
narrative_ontology:cs_drift_state('3089ce6d-f84b-4cad-899a-e2275fd732e1', contemporary_software_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3089ce6d-f84b-4cad-899a-e2275fd732e1', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_software_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive access to the full source code of any software that links to GPL-licensed components, ensuring transparency, auditability, and the freedom to modify and redistribute. Their benefit is the expansion of the commons.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_licensed_software_users, beneficiary,
    organized, generational, mobile, global).

% Actively promotes and defends the broad interpretation of 'derivative work' to ensure the copyleft mechanism functions as intended, expanding the pool of freely available software. They enforce the license through public pressure and legal action.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community, agenda_setter,
    organized, generational, mobile, global).

% Face the obligation to release their proprietary source code under GPL terms if their software links to GPL-licensed components. This imposes significant compliance costs or forces them to re-architect their products to avoid linking, impacting their business model.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Must either comply with GPL's source disclosure requirements or avoid using GPL-licensed libraries, which can limit their technical choices and increase development costs. Their options are limited by the license's viral nature.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_developers, payer,
    moderate, biographical, constrained, global).

% Advise clients on GPL compliance and litigate cases involving derivative works. Their interpretation of copyright law and case precedents shapes the practical application of this constraint.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, copyright_lawyers, observer,
    institutional, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expansion of the software commons by ensuring that contributions to GPL-licensed projects, even through linking, remain open and accessible, fostering collaborative development and preventing proprietary enclosure.
% TRANSFER_FUNCTION: Transfers the right to access, modify, and redistribute source code from proprietary developers (who link to GPL code) to the public, in exchange for the use of GPL-licensed components.
% ABSENT_VOICES: Proprietary developers who wish to use GPL-licensed components without disclosing their own source code are actively excluded from this interpretation's benefits; they would argue for a more permissive linking model.
% DISAPPEARANCE_RATIONALE: If this broad interpretation of derivative work vanished, proprietary vendors would freely link to GPL code without disclosure, leading to a significant reduction in the software commons and a shift towards more closed-source development, fundamentally altering the open-source ecosystem.
% FOUNDING_PROBLEM: Proprietary software vendors were enclosing and privatizing software innovations, limiting user freedom and hindering collaborative development, even when using publicly shared code.
% FOUNDING_PROBLEM_CORROBORATION: The open-source community and many independent developers attest that the problem of enclosure remains live, citing ongoing attempts by proprietary entities to leverage open-source components without contributing back. Legal scholars and some competition authorities also corroborate the need for mechanisms to prevent monopolization of software innovation.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).
:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.3) is moderate: it extracts source code from proprietary vendors, but this is framed as a fair exchange for using GPL components, not pure rent. Suppression (0.4) is also moderate, as it relies on legal enforcement and the threat of litigation, but developers retain the option to avoid GPL code. Theater ratio is low (0.1) as the constraint's function is direct and effective. The claimed type is 'rope' because it genuinely coordinates the expansion of the software commons, even if it imposes costs on some parties.
 *
 * PERSPECTIVAL GAP:
 *   The open-source community views this as a necessary and fair coordination mechanism to protect and expand the commons. Proprietary developers, however, experience it as a coercive extraction that limits their commercial freedom. The engine's per-seat classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The GPL-licensed software users and the open-source community are clear beneficiaries (d near 0.0), gaining access to more source code and expanding the commons. Proprietary software vendors and closed-source developers are the targets (d near 1.0), as they bear the cost of source disclosure or re-architecting. Copyright lawyers act as observers, interpreting and enforcing the constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_precedent_ambiguity,
    'How consistently do courts interpret ''derivative work'' in the context of software linking, particularly dynamic linking, across different jurisdictions?',
    'Analysis of new court rulings and legislative changes regarding software copyright and linking. A clear, consistent body of case law would reduce this ambiguity.',
    'If legal precedent consistently supports the broad reading, the constraint''s effective suppression and extractiveness would be higher and more stable. If precedent is inconsistent or favors narrower readings, the constraint''s enforceability and thus its effective metrics would decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_precedent_ambiguity, empirical, 'Uncertainty regarding the legal enforceability and scope of the broad copyleft interpretation in practice.').

omega_variable(
    technological_workaround_efficacy,
    'To what extent can proprietary developers effectively use technological workarounds (e.g., wrappers, microservices, clean-room implementations) to avoid triggering the derivative work clause while still leveraging GPL-licensed functionality?',
    'Empirical study of software architecture patterns in proprietary products that interact with GPL code, and analysis of their legal standing. The development of new, legally robust workarounds would resolve this.',
    'If workarounds are highly effective and low-cost, the constraint''s effective suppression and extractiveness on proprietary developers would be lower, as their exit options improve. If workarounds are difficult or legally risky, the constraint''s force remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_workaround_efficacy, empirical, 'The practical ability of proprietary developers to circumvent the derivative work trigger.').

omega_variable(
    conceptual_boundary_of_derivation,
    'Is the concept of ''derivative work'' in copyright law fundamentally compatible with the technical realities of software linking and modularity, or is there an irreducible conceptual tension?',
    'Ongoing legal and philosophical debate, potentially leading to new legislative definitions of ''derivative work'' for software, or a widely accepted conceptual framework that reconciles legal and technical perspectives.',
    'If a clear conceptual reconciliation emerges, the ambiguity across different readings would decrease, potentially leading to a more stable classification. If the tension is irreducible, the ''contested'' status of the kernel will persist, and different readings will continue to coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conceptual_boundary_of_derivation, conceptual, 'The fundamental conceptual compatibility between copyright''s ''derivative work'' and software linking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 20, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel. This 'broad_copyleft_reading' asserts that linking creates a derivative work, influencing the 'narrow_linking_permissive_reading' and 'interface_boundary_reading' by setting a high bar for what constitutes non-derivative use.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
