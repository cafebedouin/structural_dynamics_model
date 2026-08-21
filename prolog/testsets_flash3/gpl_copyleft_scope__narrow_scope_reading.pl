% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Copyleft Scope (Narrow Interpretation)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents a 'narrow scope' reading of the GPL's copyleft
 *   provisions, specifically Section 2(b), which interprets the 'derivative
 *   work' boundary strictly according to traditional copyright doctrine. This
 *   means that mere aggregation, plugin architectures, or certain forms of
 *   dynamic linking are generally not considered to create derivative works,
 *   thus not triggering the copyleft requirement to release the combined work
 *   under GPL. This reading provides significant flexibility for commercial
 *   firms to integrate GPL components into proprietary software, while
 *   open-source advocates perceive it as weakening the copyleft's intended
 *   reach. The constraint is claimed as a Rope, reflecting its function as a
 *   coordination mechanism for mixed codebases, but its extractiveness (0.35)
 *   reflects the 'cost' to strong copyleft advocates whose expectations of
 *   universal code-sharing are not met.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.35).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.2).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Copyleft Scope (Narrow Interpretation)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '2cee9c2b-eed0-4e16-aada-129b89dbb95d').
narrative_ontology:cs_kernel_codification('2cee9c2b-eed0-4e16-aada-129b89dbb95d', fixed_text).
narrative_ontology:cs_authority_grounding('2cee9c2b-eed0-4e16-aada-129b89dbb95d', lineage).
narrative_ontology:cs_interpretation_layer_present('2cee9c2b-eed0-4e16-aada-129b89dbb95d').
narrative_ontology:cs_reading_relation('2cee9c2b-eed0-4e16-aada-129b89dbb95d', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('2cee9c2b-eed0-4e16-aada-129b89dbb95d', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('2cee9c2b-eed0-4e16-aada-129b89dbb95d', foundational, derivative_work_follows_traditional_copyright).
narrative_ontology:cs_axiom_status(derivative_work_follows_traditional_copyright, holdable).
narrative_ontology:cs_axiom_grounding('2cee9c2b-eed0-4e16-aada-129b89dbb95d', derivative_work_follows_traditional_copyright, conventional).
narrative_ontology:cs_axiom('2cee9c2b-eed0-4e16-aada-129b89dbb95d', secondary, gpl_is_a_contract_not_a_moral_imperative).
narrative_ontology:cs_axiom_status(gpl_is_a_contract_not_a_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('2cee9c2b-eed0-4e16-aada-129b89dbb95d', gpl_is_a_contract_not_a_moral_imperative, conventional).
narrative_ontology:cs_reference_frame('2cee9c2b-eed0-4e16-aada-129b89dbb95d', traditional_copyright_doctrine).
narrative_ontology:cs_drift_state('2cee9c2b-eed0-4e16-aada-129b89dbb95d', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2cee9c2b-eed0-4e16-aada-129b89dbb95d', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, open_source_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, fsf_and_copyleft_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the flexibility to integrate GPL-licensed components into proprietary software without being forced to open-source their entire codebase, as long as the integration methods (e.g., aggregation, dynamic linking) are not considered 'derivative works' under this narrow interpretation. This allows them to leverage open-source innovation while protecting their commercial interests.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_software_firms, beneficiary,
    institutional, biographical, mobile, global).

% Benefits from the widespread adoption and integration of GPL-licensed software into various ecosystems, including commercial ones, which increases the utility and reach of their contributions. The clear, albeit narrow, boundary provides a predictable framework for collaboration.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, open_source_developers, beneficiary,
    moderate, biographical, mobile, global).

% Bears the cost of weakened copyleft enforcement, as their goal of ensuring all derivative works remain free and open-source is not fully realized. They must actively monitor and litigate to push for broader interpretations, often facing an uphill battle against established copyright doctrine and commercial interests.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, fsf_and_copyleft_advocates, payer,
    organized, generational, constrained, global).

% Interprets and applies copyright law, which forms the basis for defining 'derivative work' in this reading. Their rulings and academic discourse shape the practical scope of the GPL, often favoring traditional interpretations that limit copyleft's reach.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, legal_scholars_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for integrating open-source GPL components with other software, allowing for mixed codebases and fostering collaboration between different licensing models by defining clear boundaries for copyleft obligations.
% TRANSFER_FUNCTION: Transfers legal certainty and flexibility to commercial entities regarding their proprietary code, while transferring a more limited scope of copyleft enforcement to open-source advocates.
% ABSENT_VOICES: Developers and users who strongly advocate for a 'stronger' copyleft interpretation, believing that all forms of code coupling should trigger GPL obligations, are often marginalized in legal and commercial discourse that favors narrower interpretations. Their voices are present in advocacy but often lack decisive legal power.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished, commercial firms would face significant legal uncertainty and potential liability for integrating GPL components, likely leading to reduced adoption of GPL software in proprietary contexts. The open-source ecosystem would fragment, and new, more permissive licensing models might emerge to fill the void, fundamentally altering software development practices.
% FOUNDING_PROBLEM: The original GPL aimed to ensure software freedom by requiring derivative works to also be free, but the legal definition of 'derivative work' was ambiguous, leading to uncertainty about how GPL-licensed code could be integrated into larger systems, especially those with proprietary components.
% FOUNDING_PROBLEM_CORROBORATION: Commercial firms and many open-source developers attest that the problem of legal clarity for mixed-license projects remains live, and this reading provides a workable solution. FSF and copyleft advocates, however, contest this, arguing that the 'problem' is merely a desire to circumvent copyleft's intent, and the founding problem of ensuring software freedom is undermined by this narrow scope.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__narrow_scope_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__narrow_scope_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).
:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate because while it benefits commercial firms by allowing proprietary integration, it 'extracts' from the broader copyleft movement by limiting the scope of code that must be shared. Suppression (0.20) is low because this reading is largely sustained by prevailing legal interpretations rather than active coercion against alternative readings, though enforcement against dynamic linking patterns is rare. Theater ratio (0.10) is low as the legal interpretation is genuinely applied, not merely performed. Accessibility collapse (0.40) is moderate, as alternative licensing models exist, but this reading offers a specific, widely accepted path for mixed-license projects. Resistance (0.15) is low, primarily from copyleft advocates who push for broader interpretations, but this reading is generally accepted in commercial practice.
 *
 * PERSPECTIVAL GAP:
 *   Commercial firms perceive this as a clear, beneficial coordination mechanism that enables them to use open-source software. Copyleft advocates, however, see it as a loophole that undermines the spirit of the GPL, allowing proprietary software to 'free-ride' on open-source contributions. The engine's classification will reflect this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial software firms and many open-source developers are beneficiaries, gaining legal clarity and flexibility for integration. FSF and copyleft advocates are payers, as their vision of pervasive copyleft is curtailed. Legal scholars and courts act as agenda-setters, shaping the interpretation. No explicit 'victims' are declared because this reading, while limiting, is not seen as actively harming any party in a way that suppresses their existence, but rather as defining the boundaries of a coordination mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Does ''derivative work'' in copyright law adequately capture the technical realities of modern software coupling (e.g., dynamic linking, plugin architectures)?',
    'New legislation specifically defining software derivative works, or landmark court cases establishing clear precedents for various coupling methods.',
    'If traditional copyright doctrine is deemed insufficient, the narrow scope reading might be challenged, potentially leading to a broader interpretation of copyleft and increased extractiveness for commercial firms. If deemed sufficient, the current reading is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, empirical, 'Ambiguity in applying traditional copyright''s ''derivative work'' concept to software.').

omega_variable(
    enforcement_capacity_of_copyleft_advocates,
    'What is the actual capacity of FSF and other copyleft advocates to enforce a broader interpretation of GPL Section 2(b) through litigation or other means?',
    'Analysis of successful and unsuccessful enforcement actions, funding for legal efforts, and industry response to such actions.',
    'If enforcement capacity is low, the ''narrow scope'' reading effectively becomes an ''enforcement vacuum'' reading, where the constraint''s actual impact is minimal regardless of legal theory. If high, the extractiveness for commercial firms could increase as they face greater pressure to comply with broader interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_of_copyleft_advocates, empirical, 'The practical ability of copyleft advocates to enforce their preferred interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'gpl_copyleft_scope' kernel. This 'narrow_scope_reading' emphasizes traditional copyright doctrine, contrasting with the 'strong_copyleft_reading' (which seeks broader application) and the 'enforcement_vacuum_reading' (which highlights the practical limits of enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
