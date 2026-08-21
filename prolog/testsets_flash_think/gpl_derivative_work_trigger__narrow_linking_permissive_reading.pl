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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: Software Licensing / Copyright Law / Open Source Governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'narrow linking permissive' reading of
 *   the `gpl_derivative_work_trigger` kernel, which asserts that linking with
 *   GPL code does not automatically create a derivative work, thus limiting
 *   the scope of copyleft obligations. Sibling readings include
 *   `broad_copyleft_reading` and `interface_boundary_reading`. This reading
 *   creates a 'wall' protecting proprietary modules, frustrating the FSF's
 *   propagation goal and causing users of those modules to lose the
 *   source-availability guarantee.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.65).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.55).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Software Licensing / Copyright Law / Open Source Governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '7e1be792-e80b-4d57-b511-bca28f6d21bb').
narrative_ontology:cs_kernel_codification('7e1be792-e80b-4d57-b511-bca28f6d21bb', fixed_text).
narrative_ontology:cs_authority_grounding('7e1be792-e80b-4d57-b511-bca28f6d21bb', practice).
narrative_ontology:cs_interpretation_layer_present('7e1be792-e80b-4d57-b511-bca28f6d21bb').
narrative_ontology:cs_reading_relation('7e1be792-e80b-4d57-b511-bca28f6d21bb', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('7e1be792-e80b-4d57-b511-bca28f6d21bb', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('7e1be792-e80b-4d57-b511-bca28f6d21bb', foundational, linking_is_aggregation).
narrative_ontology:cs_axiom_status(linking_is_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('7e1be792-e80b-4d57-b511-bca28f6d21bb', linking_is_aggregation, conventional).
narrative_ontology:cs_axiom('7e1be792-e80b-4d57-b511-bca28f6d21bb', secondary, independent_work_status).
narrative_ontology:cs_axiom_status(independent_work_status, holdable).
narrative_ontology:cs_axiom_grounding('7e1be792-e80b-4d57-b511-bca28f6d21bb', independent_work_status, conventional).
narrative_ontology:cs_reference_frame('7e1be792-e80b-4d57-b511-bca28f6d21bb', copyright_aggregation_principle).
narrative_ontology:cs_drift_state('7e1be792-e80b-4d57-b511-bca28f6d21bb', contemporary_software_development, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7e1be792-e80b-4d57-b511-bca28f6d21bb', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_entities_using_gpl).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and benefit from this interpretation, as it allows them to link their proprietary code with GPL-licensed libraries without being obligated to release their own source code. They actively fund legal defense and lobbying efforts to maintain this reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers, agenda_setter,
    powerful, biographical, mobile, global).

% Utilize GPL-licensed components in their products, relying on this interpretation to protect their proprietary intellectual property. Their business models depend on avoiding broader copyleft obligations.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_entities_using_gpl, beneficiary,
    organized, biographical, constrained, global).

% Are the intended beneficiaries of the GPL's propagation goals, but under this reading, they lose the guarantee of source code availability for proprietary modules linked to GPL software. They bear the cost of reduced transparency and freedom.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_users, payer,
    moderate, biographical, constrained, global).

% The primary institutional advocate for strong copyleft, viewing this interpretation as a direct frustration of the GPL's core mission to ensure software freedom. They expend significant resources on legal education and enforcement efforts against this reading.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, payer,
    institutional, generational, identity_locked, global).

% Support the FSF's goals and view this interpretation as undermining the principles of open source. While not directly paying, they are excluded from the full benefits of copyleft's intended propagation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_advocates, excluded,
    organized, generational, constrained, global).

% Interpret and apply copyright law, including the GPL. Their rulings and legal arguments shape the practical application and persistence of this interpretation. They are the arbiters of the ongoing debate.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyright_lawyers_and_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__narrow_linking_permissive_reading, copyright_lawyers_and_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_developers).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework that allows proprietary software to interoperate with GPL-licensed components by defining specific conditions under which linking does not trigger derivative work obligations, thereby facilitating broader software ecosystem integration.
% TRANSFER_FUNCTION: Transfers the obligation to share source code from proprietary developers (who link to GPL code) to no one, effectively allowing them to retain proprietary control and extract value from their closed-source modules while using GPL components.
% ABSENT_VOICES: The original authors of GPL code who intended broader propagation, and users who expect full source availability for all linked components, are structurally marginalized in the legal discourse that favors proprietary interests.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, proprietary software developers would face immediate and severe legal risks for their existing products. They would be forced to either re-architect their software to avoid linking GPL components, acquire commercial licenses, or open-source their proprietary modules, leading to a significant reorganization of the software industry.
% FOUNDING_PROBLEM: The core problem was defining the boundary of 'derivative work' in copyright law, specifically how linking different software modules affects their legal status, balancing the rights of copyright holders with the desire for software interoperability and commercial innovation.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, software industry analysts, and various open-source organizations (including but not limited to the FSF) outside the direct beneficiaries acknowledge that the definition of 'derivative work' in the context of linking remains a live and contentious legal issue, with significant economic implications.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high from the perspective of GPL users and advocates, as it allows proprietary entities to derive significant value from GPL code without reciprocating. Suppression (0.55) reflects the legal and technical barriers that prevent users from accessing the source code of linked proprietary modules. The theater ratio is low (0.10) because this is a genuine legal interpretation with real-world consequences, not a performance. The increasing extractiveness over time reflects the growing reliance of proprietary software on open-source components, making the 'linking is aggregation' argument more valuable to commercial entities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of proprietary developers, this interpretation is a reasonable application of copyright law that fosters innovation and interoperability. From the perspective of the FSF and GPL users, it is an extractive loophole that undermines the core principles of copyleft. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software developers and commercial entities are clear beneficiaries (low d) as this interpretation directly enables their business models. GPL users, the Free Software Foundation, and open-source advocates are targets (high d) as their goals of software freedom and source availability are frustrated. Copyright lawyers and courts act as observers and agenda-setters, shaping the legal landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Is the legal definition of ''derivative work'' sufficiently clear to unambiguously distinguish between linking and modification in software contexts?',
    'A landmark court ruling or legislative action that provides a definitive, universally accepted definition of ''derivative work'' in software linking scenarios.',
    'If resolved towards a stricter definition, the extractiveness of this reading would increase for proprietary developers (as they would face more obligations) and decrease for GPL users. If resolved towards a more permissive definition, the current extractiveness profile would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in the legal definition of ''derivative work'' for software.').

omega_variable(
    fsf_propagation_goal_quantification,
    'To what extent can the ''frustration'' of the FSF''s propagation goal be quantitatively measured in terms of lost source code availability or reduced software freedom?',
    'Development of a robust, independently verifiable metric for software freedom or source code propagation that can be applied across different licensing interpretations.',
    'A clear quantification would solidify the ''victim'' status of GPL users and the FSF, potentially increasing the measured extractiveness and suppression if the impact is severe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fsf_propagation_goal_quantification, empirical, 'Quantifying the impact on FSF''s propagation goal.').

omega_variable(
    linking_vs_modification_conceptual_boundary,
    'Is the conceptual distinction between ''linking'' and ''modification'' in software development a robust and universally applicable boundary, or is it context-dependent and fluid?',
    'Consensus among computer scientists and legal experts on a formal, technical definition that clearly delineates linking from modification across all programming paradigms and architectural styles.',
    'If the boundary is found to be fluid, this reading''s claim of ''aggregation not derivation'' becomes weaker, potentially shifting its classification towards a Snare if the coordination function is deemed a cover for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_vs_modification_conceptual_boundary, conceptual, 'Conceptual robustness of linking vs. modification distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t2000, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(gpl__tr_t2005, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2005, 0.09).
narrative_ontology:measurement(gpl__tr_t2010, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gpl__tr_t2020, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(gpl__tr_t2025, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2025, 0.1).
narrative_ontology:measurement(gpl__tr_t2030, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t2000, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(gpl__be_t2005, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(gpl__be_t2010, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(gpl__be_t2015, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(gpl__be_t2020, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2020, 0.64).
narrative_ontology:measurement(gpl__be_t2025, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement(gpl__be_t2030, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 2030, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t2000, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(gpl__su_t2005, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2005, 0.48).
narrative_ontology:measurement(gpl__su_t2010, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(gpl__su_t2015, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(gpl__su_t2020, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2020, 0.54).
narrative_ontology:measurement(gpl__su_t2025, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2025, 0.55).
narrative_ontology:measurement(gpl__su_t2030, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 2030, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
