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
 *   This constraint represents a 'narrow scope' reading of GPL Section 2(b),
 *   asserting that copyleft obligations apply only to direct derivative
 *   works, excluding mere aggregation, plugin architectures, and certain
 *   dynamic linking forms. This interpretation aligns with traditional
 *   copyright doctrine and allows commercial firms to integrate GPL
 *   components with proprietary layers. It is classified as a Rope because it
 *   facilitates coordination for mixed codebases, but with a moderate epsilon
 *   due to the perceived 'extraction' of flexibility from the strong copyleft
 *   intent.
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
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Copyleft Scope (Narrow Interpretation)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, 'dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f').
narrative_ontology:cs_kernel_codification('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', fixed_text).
narrative_ontology:cs_authority_grounding('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', practice).
narrative_ontology:cs_interpretation_layer_present('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f').
narrative_ontology:cs_reading_relation('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', foundational, copyright_doctrine_limits_copyleft).
narrative_ontology:cs_axiom_status(copyright_doctrine_limits_copyleft, holdable).
narrative_ontology:cs_axiom_grounding('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', copyright_doctrine_limits_copyleft, conventional).
narrative_ontology:cs_axiom('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', secondary, aggregation_is_not_derivation).
narrative_ontology:cs_axiom_status(aggregation_is_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', aggregation_is_not_derivation, conventional).
narrative_ontology:cs_reference_frame('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', traditional_copyright_principles).
narrative_ontology:cs_drift_state('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dae24b40-b3fb-4ca6-8909-8b78ac5d5f9f', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_firms).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These firms benefit from the flexibility to integrate GPL-licensed components into their proprietary software products without being forced to open-source their entire codebase. This interpretation allows them to maintain proprietary layers while leveraging open-source innovation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, commercial_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Individual developers working for commercial entities or on their own proprietary projects find this interpretation favorable, as it reduces the legal complexity and risk associated with using GPL components, allowing for broader adoption in mixed-license environments.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, proprietary_developers, beneficiary,
    moderate, biographical, mobile, global).

% These are the creators of the GPL-licensed software components. While they set the license terms, this narrow interpretation limits the reach of their copyleft intent, meaning less code is reciprocally shared than they might desire. They are the nominal agenda-setters but their interpretation is contested.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, gpl_component_developers, agenda_setter,
    organized, generational, constrained, global).

% Organizations and individuals who champion the strong copyleft philosophy, aiming for maximal code freedom and reciprocal sharing. This narrow interpretation weakens their ability to enforce broad copyleft, leading to a perceived 'loss' of potential open-source contributions.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).

% Academics and legal experts who analyze copyright law and its application to software licenses. They contribute to the discourse on derivative works and copyleft scope, influencing but not directly enforcing the constraint.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__narrow_scope_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for integrating GPL-licensed open-source components into larger software projects, facilitating the creation of mixed-license codebases and reducing legal uncertainty for developers.
% TRANSFER_FUNCTION: Transfers flexibility and proprietary control to commercial entities and proprietary developers, allowing them to leverage open-source code without fully open-sourcing their own. This comes at the cost of reduced scope for universal code-sharing, which is the goal of strong copyleft advocates.
% ABSENT_VOICES: Developers and organizations who strictly adhere to a strong copyleft interpretation and believe that any form of code coupling (including dynamic linking) should trigger copyleft. They would argue for a broader definition of 'derivative work' to ensure more code remains free.
% DISAPPEARANCE_RATIONALE: If this narrow interpretation vanished overnight, commercial firms and proprietary developers would face immense legal uncertainty and potential liability, forcing them to either fully open-source large portions of their code or abandon GPL components. This would drastically alter software development practices, business models, and the overall open-source ecosystem.
% FOUNDING_PROBLEM: The original GPL was created to ensure that software distributed under it, and its derivative works, would remain free, preventing proprietary enclosure and fostering a vibrant free software ecosystem.
% FOUNDING_PROBLEM_CORROBORATION: Commercial firms and proprietary developers attest that this narrow scope is a pragmatic necessity for integrating open-source components into complex commercial products, allowing for broader adoption of GPL code. Copyleft advocates and some legal scholars attest that the original intent was broader, and the problem of proprietary enclosure persists due to this narrow reading, citing the ongoing debate over 'derivative work' definitions.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__narrow_scope_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__narrow_scope_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__narrow_scope_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness (0.35) is moderate, reflecting the 'moderate-epsilon rope' description. While it enables coordination, it does so by limiting the reach of copyleft, which is seen as an 'extraction' of potential open-source contributions by copyleft advocates. Suppression (0.20) is low because this reading is widely accepted in industry, and enforcement against dynamic linking patterns is rare. Theater ratio (0.10) is low as the constraint is functional in providing legal clarity. Accessibility collapse (0.30) is low as alternative licenses and proprietary development paths remain viable. Resistance (0.25) is low because many stakeholders benefit from this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Commercial firms and proprietary developers perceive this as a beneficial coordination mechanism, enabling them to use open-source components effectively. Copyleft advocates, however, view it as a weakening of the GPL's core intent, leading to less code being truly 'free'. The engine's classification as a Rope reflects the coordination function, while the moderate extractiveness captures the cost borne by copyleft advocates.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial firms and proprietary developers are beneficiaries (low d) as they gain flexibility and reduced legal risk. GPL component developers are agenda-setters, but their original intent is partially undermined, placing them closer to symmetric. Copyleft advocates are payers (high d) as their goal of universal code-sharing is constrained by this interpretation. Legal scholars are observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_legal_definition,
    'What is the definitive legal definition of a ''derivative work'' in the context of software, particularly concerning linking and aggregation?',
    'A landmark judicial ruling from a high court specifically addressing dynamic linking, plugin architectures, and aggregation under copyright law and the GPL.',
    'A broad definition would strengthen the ''strong_copyleft_reading'' and increase extractiveness for commercial firms; a narrow definition would solidify this ''narrow_scope_reading'' and reduce perceived extraction for beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_legal_definition, empirical, 'Ambiguity in the legal boundary of ''derivative work'' for software.').

omega_variable(
    gpl_original_intent_vs_current_practice,
    'To what extent does the current industry practice and legal interpretation of GPL Section 2(b) diverge from the original intent of the Free Software Foundation?',
    'Historical analysis of FSF statements and legal interpretations, combined with surveys of developer and corporate licensing practices over time.',
    'If significant divergence is found, it would highlight a ''practice_drift'' in the ''strong_copyleft_reading'' and potentially increase the perceived extraction for copyleft advocates under this ''narrow_scope_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_original_intent_vs_current_practice, conceptual, 'Gap between GPL''s original intent and its practical application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 2000, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(gpl__tr_t2005, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(gpl__tr_t2010, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gpl__tr_t2020, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(gpl__tr_t2025, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(gpl__be_t2005, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2005, 0.32).
narrative_ontology:measurement(gpl__be_t2010, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(gpl__be_t2015, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2015, 0.34).
narrative_ontology:measurement(gpl__be_t2020, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2020, 0.35).
narrative_ontology:measurement(gpl__be_t2025, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(gpl__su_t2005, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2005, 0.19).
narrative_ontology:measurement(gpl__su_t2010, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(gpl__su_t2015, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2015, 0.2).
narrative_ontology:measurement(gpl__su_t2020, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2020, 0.2).
narrative_ontology:measurement(gpl__su_t2025, gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, information_standard).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel, each representing a distinct interpretation of GPL Section 2(b)'s scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
