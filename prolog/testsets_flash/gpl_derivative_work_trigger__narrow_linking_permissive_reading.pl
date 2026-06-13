% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents a permissive interpretation of the GNU General
 *   Public License (GPL) regarding derivative works. Under this reading,
 *   merely linking to GPL-licensed code (especially dynamically) is
 *   considered aggregation, not derivation, and therefore does not trigger
 *   the GPL's copyleft obligations for the linking proprietary code. Only
 *   direct modification of the GPL code itself creates a derivative work.
 *   This interpretation is favored by proprietary software developers seeking
 *   to integrate open-source components without open-sourcing their entire
 *   codebase.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.45).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.3).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '6c1eff13-4a32-4bff-915d-45db21bdafe3').
narrative_ontology:cs_kernel_codification('6c1eff13-4a32-4bff-915d-45db21bdafe3', fixed_text).
narrative_ontology:cs_authority_grounding('6c1eff13-4a32-4bff-915d-45db21bdafe3', lineage).
narrative_ontology:cs_interpretation_layer_present('6c1eff13-4a32-4bff-915d-45db21bdafe3').
narrative_ontology:cs_reading_relation('6c1eff13-4a32-4bff-915d-45db21bdafe3', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c1eff13-4a32-4bff-915d-45db21bdafe3', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('6c1eff13-4a32-4bff-915d-45db21bdafe3', foundational, linking_is_aggregation).
narrative_ontology:cs_axiom_status(linking_is_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('6c1eff13-4a32-4bff-915d-45db21bdafe3', linking_is_aggregation, conventional).
narrative_ontology:cs_axiom('6c1eff13-4a32-4bff-915d-45db21bdafe3', foundational, modification_triggers_obligation).
narrative_ontology:cs_axiom_status(modification_triggers_obligation, holdable).
narrative_ontology:cs_axiom_grounding('6c1eff13-4a32-4bff-915d-45db21bdafe3', modification_triggers_obligation, conventional).
narrative_ontology:cs_reference_frame('6c1eff13-4a32-4bff-915d-45db21bdafe3', copyright_law_traditional_aggregation).
narrative_ontology:cs_drift_state('6c1eff13-4a32-4bff-915d-45db21bdafe3', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6c1eff13-4a32-4bff-915d-45db21bdafe3', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_users_of_gpl_software).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These developers benefit by being able to use GPL-licensed libraries and components in their proprietary software without being forced to open-source their entire product. They actively advocate for this permissive interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers, beneficiary,
    powerful, biographical, mobile, global).

% Companies that use GPL software in their products or services, but wish to keep their value-added layers proprietary. This reading reduces their legal risk and compliance burden.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_users_of_gpl_software, beneficiary,
    organized, biographical, constrained, global).

% The primary advocate for the GPL and its strong copyleft provisions. This reading undermines their goal of ensuring that all software built upon GPL code remains free, as it creates a 'wall' for proprietary modules. They actively litigate against this interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, payer,
    institutional, generational, identity_locked, global).

% Individual developers and organizations who believe in the strong copyleft principles of the GPL. They see this permissive reading as a weakening of the license's intent and a threat to the free software ecosystem.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_advocates, payer,
    moderate, generational, identity_locked, global).

% Courts and legal bodies that interpret copyright law and specific license terms. Their rulings determine which interpretation of 'derivative work' prevails, thereby shaping the practical effect of the GPL.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for proprietary and open-source software to coexist and interoperate, allowing proprietary developers to utilize open-source components without fully adopting copyleft obligations.
% TRANSFER_FUNCTION: Transfers the benefit of using GPL-licensed code to proprietary developers without requiring them to contribute their own code back under GPL terms, effectively transferring potential 'free' status from derivative works to proprietary status.
% ABSENT_VOICES: The original authors of GPL-licensed code who intended a broader copyleft effect, and the broader free software community who rely on strong copyleft for the growth of the free software ecosystem, are often not directly represented in the legal interpretations that favor this reading.
% DISAPPEARANCE_RATIONALE: If this permissive interpretation vanished, proprietary software developers would face significantly higher legal risks and compliance costs when using GPL components, potentially leading to a reduction in GPL adoption in commercial products or a shift towards more permissive licenses. The software industry's integration patterns would fundamentally change.
% FOUNDING_PROBLEM: The challenge of integrating open-source software into proprietary products without triggering unwanted license obligations, balancing the benefits of open source with the need to protect intellectual property.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software companies and their legal counsel consistently attest to this problem in industry forums and legal discussions. The FSF acknowledges the existence of this tension, though they dispute the legitimacy of this particular 'solution'.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).
:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as it allows proprietary developers to leverage GPL code without fully reciprocating, effectively 'extracting' value from the open-source ecosystem without contributing back under the same terms. Suppression is low (0.30) because this reading is a legal interpretation, not an actively enforced coercive mechanism; its persistence relies on legal arguments and the absence of definitive counter-rulings. Theater ratio is low (0.10) as there's little performative maintenance; the constraint is upheld through legal interpretation rather than theatrical displays.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary developers, this is a reasonable interpretation that balances open-source use with commercial interests. From the FSF's perspective, it's a loophole that undermines the core intent of the GPL. The engine will compute different classifications for these seats based on their declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software developers and commercial users of GPL software are beneficiaries (d near 0.0) as this reading allows them to protect their proprietary code while using GPL components. The Free Software Foundation (FSF) and GPL advocates are victims (d near 1.0) as this reading frustrates their goal of maximizing the propagation of free software. The legal system acts as an agenda-setter, adjudicating disputes and shaping the interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Is ''linking'' (especially dynamic linking) a form of ''derivation'' under copyright law, or merely ''aggregation''?',
    'Definitive court rulings on specific linking scenarios, or legislative clarification of ''derivative work'' in the context of software.',
    'If linking is derivation, this reading is overridden, and the GPL''s copyleft effect is significantly broadened, increasing obligations for proprietary developers. If linking is aggregation, this reading is reinforced, limiting GPL''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in copyright law''s definition of ''derivative work'' as applied to software linking.').

omega_variable(
    gpl_kernel_reading_divergence,
    'This constraint is one reading of the ''gpl_derivative_work_trigger'' kernel. What would change if the ''broad_copyleft_reading'' or ''interface_boundary_reading'' were adopted?',
    'Legal precedent or widespread industry adoption of a different interpretation.',
    'The ''broad_copyleft_reading'' would increase obligations for proprietary developers and expand the scope of GPL. The ''interface_boundary_reading'' would create a different set of conditions for non-derivation, potentially allowing more proprietary integration than this reading, but with stricter API separation requirements. This reading protects proprietary modules and frustrates the FSF''s propagation goal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gpl_kernel_reading_divergence, conceptual, 'The ''gpl_derivative_work_trigger'' kernel has multiple contested readings, each with different implications for software licensing and proprietary integration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gpl__tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(gpl__be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(gpl__su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, information_standard).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel, each representing a different interpretation of what constitutes a 'derivative work' under the GPL. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
