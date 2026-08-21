% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__interface_boundary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__interface_boundary_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger: Interface Boundary Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'interface boundary' reading of the GPL's
 *   derivative work trigger, a key point of contention in software licensing.
 *   This reading asserts that if software components interact solely through
 *   clean, well-defined APIs, they constitute aggregation rather than
 *   derivation, thus not triggering the GPL's copyleft obligations on the
 *   non-GPL component. This interpretation facilitates modular architecture
 *   and mixed-licensing models but is contested by those advocating for a
 *   broader scope of copyleft. The prompt's 'expected structural delta' of
 *   'Scaffold allowing modular architecture with mixed licensing' describes
 *   the *effect* of this reading, but the reading itself, as a standing legal
 *   interpretation, is structurally a Tangled Rope due to its coordination
 *   function (enabling mixed licensing) combined with asymmetric extraction
 *   (from users/copyleft advocates) and active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.65).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.7).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger: Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, 'ce96531c-f7a2-4c6e-8650-4313ab899a81').
narrative_ontology:cs_kernel_codification('ce96531c-f7a2-4c6e-8650-4313ab899a81', fixed_text).
narrative_ontology:cs_authority_grounding('ce96531c-f7a2-4c6e-8650-4313ab899a81', lineage).
narrative_ontology:cs_interpretation_layer_present('ce96531c-f7a2-4c6e-8650-4313ab899a81').
narrative_ontology:cs_reading_relation('ce96531c-f7a2-4c6e-8650-4313ab899a81', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('ce96531c-f7a2-4c6e-8650-4313ab899a81', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('ce96531c-f7a2-4c6e-8650-4313ab899a81', foundational, api_linkage_is_aggregation).
narrative_ontology:cs_axiom_status(api_linkage_is_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('ce96531c-f7a2-4c6e-8650-4313ab899a81', api_linkage_is_aggregation, conventional).
narrative_ontology:cs_axiom('ce96531c-f7a2-4c6e-8650-4313ab899a81', foundational, clean_api_boundary_is_non_derivative).
narrative_ontology:cs_axiom_status(clean_api_boundary_is_non_derivative, holdable).
narrative_ontology:cs_axiom_grounding('ce96531c-f7a2-4c6e-8650-4313ab899a81', clean_api_boundary_is_non_derivative, conventional).
narrative_ontology:cs_reference_frame('ce96531c-f7a2-4c6e-8650-4313ab899a81', modular_interoperability_framework).
narrative_ontology:cs_drift_state('ce96531c-f7a2-4c6e-8650-4313ab899a81', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ce96531c-f7a2-4c6e-8650-4313ab899a81', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_advocates).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_full_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and benefit from this interpretation, as it allows them to combine GPL-licensed components with proprietary code via clean API boundaries without triggering full copyleft obligations. They shape legal arguments and community norms.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the flexibility this interpretation provides, allowing them to leverage open-source libraries without having to open-source their entire product. This reduces development costs and increases market reach.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Bear the cost of reduced copyleft scope, as this interpretation limits the 'viral' effect of the GPL. They actively resist this reading through legal challenges, community education, and promoting alternative interpretations.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, copyleft_advocates, payer,
    organized, generational, constrained, global).

% Lose access to the full source code for components that are tightly coupled via APIs but deemed non-derivative under this reading. Their expectation of full software freedom is curtailed by this legal boundary.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, users_expecting_full_source, payer,
    powerless, biographical, constrained, global).

% Monitor the legal landscape, provide guidance to developers, and sometimes engage in litigation to clarify or challenge interpretations of copyleft licenses. They represent a broad range of interests within the open-source community.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, open_source_foundations, observer,
    institutional, generational, analytical, global).

% Adjudicate disputes and publish scholarly articles that shape the understanding and application of copyright law, including the definition of derivative works in software. Their rulings and interpretations provide the ultimate enforcement mechanism.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts_and_legal_scholars, agenda_setter,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__interface_boundary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates interoperability and modular software development by providing a clear, predictable boundary for what constitutes a 'derivative work' when linking to copylefted libraries via well-defined APIs, thereby enabling mixed-licensing ecosystems.
% TRANSFER_FUNCTION: Transfers the obligation to provide source code for API-linked components from proprietary software vendors to users/copyleft advocates, who would otherwise receive it under broader interpretations of 'derivative work'.
% ABSENT_VOICES: Users who are not technically or legally sophisticated enough to understand the nuances of API boundaries and licensing, and who simply expect 'free software' to mean full source access for all components of a linked system. Their expectations are often unrepresented in legal discourse.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal landscape for software linking would become highly uncertain. This would likely lead to a chilling effect on mixed-licensing projects, a resurgence of broader copyleft interpretations, and force many software projects to re-evaluate their entire licensing and architectural strategies, fundamentally reorganizing the mobile and enterprise software economies.
% FOUNDING_PROBLEM: The need to define the scope of 'derivative work' in copyright law for software that links to copylefted libraries via well-defined interfaces, balancing the goals of software freedom and interoperability.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, software developers, and industry bodies outside of direct beneficiaries (e.g., academic researchers in intellectual property law, independent software architects) acknowledge the ongoing challenge of defining derivative works in complex, modular software ecosystems.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__interface_boundary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because this interpretation allows proprietary software to leverage GPL-licensed code without contributing back, effectively extracting the 'freedom' that copyleft aims to guarantee for users. Suppression is high (0.70) as this interpretation is actively defended in legal contexts and through community pressure, suppressing alternative, broader interpretations. Theater ratio is low (0.20) because the interpretation has real, non-performative legal consequences. Accessibility collapse is moderate (0.60) as it limits the alternative of demanding full source for API-linked components. Resistance is also moderate (0.55) due to ongoing advocacy and legal challenges from copyleft proponents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecosystem integrators, this interpretation is a necessary coordination mechanism for modern modular software development. From the perspective of copyleft advocates, it's an extractive loophole that undermines the core principles of software freedom. The engine's classification as Tangled Rope captures this dual nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators and proprietary software vendors are clear beneficiaries, gaining flexibility and market advantage. Copyleft advocates and users expecting full source are the primary targets/payers, as their desired outcome (full source availability) is curtailed. Open source foundations and courts act as observers and agenda-setters, shaping the interpretation and its enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''interface_boundary_reading'' of the ''gpl_derivative_work_trigger'' kernel?',
    'Analysis of legal precedents and community consensus on the specific interpretation of GPL linking rules.',
    'If misidentified, the entire analysis of its relations to sibling readings and its internal axioms would be flawed, leading to incorrect classification and drift detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being analyzed within the GPL derivative work kernel.').

omega_variable(
    api_cleanliness_ambiguity,
    'What constitutes a ''clean API boundary'' in practice, and how consistently is this definition applied across different software architectures and legal jurisdictions?',
    'Empirical study of court rulings, licensing agreements, and developer practices to identify common criteria and points of contention for ''clean API'' definitions.',
    'If the definition of ''clean API'' is highly ambiguous or inconsistently applied, the constraint''s effective extractiveness and suppression could vary significantly, potentially shifting its classification towards a Snare for those caught in the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(api_cleanliness_ambiguity, empirical, 'Ambiguity in the practical definition of ''clean API boundary''.').

omega_variable(
    structural_delta_broad_reading,
    'What would be the structural impact on the software ecosystem if the ''broad_copyleft_reading'' of the GPL derivative work trigger were universally adopted?',
    'Counterfactual modeling of software development and licensing trends under the broad reading, including analysis of project abandonment, re-licensing efforts, and new open-source contributions.',
    'If the broad reading were adopted, the extractiveness of this constraint would drop to near zero, and the beneficiaries would become victims, as their current practices would be deemed non-compliant. This would fundamentally alter the balance of power in software licensing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_delta_broad_reading, conceptual, 'Impact of adopting the broad copyleft reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__interface_boundary_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel, each representing a distinct interpretation of what constitutes a derivative work under the GPL. This reading focuses on API boundaries as the determinant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
