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
 *   constraint_id: gpl_derivative_work_trigger__interface_boundary_reading
 *   human_readable: GPL Derivative Work Trigger: Interface Boundary Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'interface boundary' reading of the GPL's
 *   derivative work trigger, asserting that clean API boundaries constitute
 *   non-derivative aggregation even with tight coupling. This interpretation
 *   allows for modular software architectures and mixed licensing, where
 *   proprietary code can link to GPL libraries without becoming subject to
 *   the GPL's copyleft. It is a contested reading within the open-source
 *   community and legal scholarship, standing in opposition to broader
 *   interpretations of copyleft.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__interface_boundary_reading, 0.6).
domain_priors:suppression_score(gpl_derivative_work_trigger__interface_boundary_reading, 0.7).
domain_priors:theater_ratio(gpl_derivative_work_trigger__interface_boundary_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__interface_boundary_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__interface_boundary_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__interface_boundary_reading, "GPL Derivative Work Trigger: Interface Boundary Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__interface_boundary_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__interface_boundary_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__interface_boundary_reading, '848d6a36-d8c0-4e0b-ac5b-8408888e9ac3').
narrative_ontology:cs_kernel_codification('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', fixed_text).
narrative_ontology:cs_authority_grounding('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', practice).
narrative_ontology:cs_interpretation_layer_present('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3').
narrative_ontology:cs_reading_relation('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_axiom('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', foundational, api_boundary_is_seam).
narrative_ontology:cs_axiom_status(api_boundary_is_seam, holdable).
narrative_ontology:cs_axiom_grounding('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', api_boundary_is_seam, conventional).
narrative_ontology:cs_axiom('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', foundational, independent_work_status_by_api).
narrative_ontology:cs_axiom_status(independent_work_status_by_api, holdable).
narrative_ontology:cs_axiom_grounding('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', independent_work_status_by_api, conventional).
narrative_ontology:cs_reference_frame('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', modular_licensing_compatibility).
narrative_ontology:cs_drift_state('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', contemporary_software_ecosystem, gap(stable, minor, true)).
narrative_ontology:cs_created_at('848d6a36-d8c0-4e0b-ac5b-8408888e9ac3', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__interface_boundary_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__interface_boundary_reading, commercial_software_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, gpl_advocates).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__interface_boundary_reading, open_source_users_expecting_full_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from this interpretation by being able to use GPL-licensed libraries via clean API boundaries without their entire codebase becoming subject to the GPL's copyleft provisions, enabling mixed-licensing architectures.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, ecosystem_integrators, beneficiary,
    powerful, biographical, arbitrage, global).

% Leverages this reading to integrate open-source components into proprietary products, reducing development costs and increasing market reach without compromising intellectual property.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, commercial_software_developers, beneficiary,
    powerful, biographical, mobile, global).

% Bears the cost of this interpretation as it limits the 'viral' effect of the GPL, potentially undermining the goal of ensuring all derivative works are free software. They actively resist this reading through legal challenges and public discourse.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, gpl_advocates, payer,
    organized, generational, constrained, global).

% Are victims of this reading when they receive software that incorporates GPL components via API boundaries but does not provide source code for the entire application, contrary to their expectation of full software freedom.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, open_source_users_expecting_full_source, payer,
    powerless, immediate, trapped, global).

% As the primary enforcer and interpreter of the GPL, the FSF legal team actively contests this reading, advocating for a broader interpretation of 'derivative work' to maximize copyleft's reach.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, fsf_legal_team, agenda_setter,
    institutional, generational, analytical, global).

% Adjudicate disputes over GPL compliance and the definition of 'derivative work'. Their rulings can either reinforce or challenge this interpretation, shaping its future trajectory.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__interface_boundary_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates software development by providing a specific (to this reading) boundary for what constitutes a 'derivative work' under the GPL, thereby enabling modular architectures and mixed licensing models for software projects.
% TRANSFER_FUNCTION: Transfers the obligation to provide source code (or the right to demand it) from certain linked components, allowing proprietary components to coexist with GPL libraries without full copyleft infection, from commercial developers to users and GPL advocates.
% ABSENT_VOICES: Users who are not technically savvy enough to understand the nuances of API linking but simply expect all linked code to be open source. They would object to the limited scope of copyleft this reading implies.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the legal landscape for software linking would become highly uncertain, forcing many projects to re-evaluate their licensing, potentially leading to widespread re-licensing, abandonment of modular architectures, or increased litigation. The software ecosystem would reorganize around a more restrictive understanding of derivative works.
% FOUNDING_PROBLEM: The original GPL aimed to ensure software freedom, but the definition of 'derivative work' for linking was ambiguous, leading to legal uncertainty for developers wanting to use GPL components in larger systems while maintaining distinct licensing for other parts.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and industry bodies outside the FSF acknowledge the ongoing ambiguity and the need for clear boundaries in software licensing to foster innovation and interoperability, supporting the claim that the problem is still live.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__interface_boundary_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__interface_boundary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__interface_boundary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_derivative_work_trigger__interface_boundary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__interface_boundary_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Tangled Rope because it provides a coordination function (enabling mixed licensing and modularity) but also involves significant extraction from users expecting full source code and from GPL advocates who see its core principles undermined. Active enforcement is required through legal challenges and licensing agreements to maintain this interpretation against competing views. Extractiveness and suppression are high due to the ongoing contestation and the perceived 'loss' of freedom for some parties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ecosystem integrators, this reading is a beneficial coordination mechanism that enables innovation and interoperability. From the perspective of GPL advocates, it is an extractive mechanism that weakens the copyleft principle and undermines software freedom. The engine's classification as Tangled Rope reflects this inherent tension and asymmetric experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecosystem integrators and commercial software developers are beneficiaries, as this reading reduces their licensing obligations and increases flexibility. GPL advocates and open-source users expecting full source are victims, as they experience a reduction in the scope of copyleft and the availability of full source code. The FSF legal team acts as an agenda-setter, actively shaping and enforcing their preferred interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_definition_ambiguity,
    'Is the definition of ''derivative work'' under copyright law primarily a technical question (how code interacts) or a legal question (intent and control)?',
    'A definitive, widely accepted judicial ruling that clarifies the technical vs. legal weighting in determining derivative status for software linking.',
    'If primarily technical, this reading gains strength; if primarily legal, the intent of copyleft (as interpreted by the FSF) might take precedence, weakening this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_definition_ambiguity, conceptual, 'Ambiguity in the foundational definition of ''derivative work'' for software.').

omega_variable(
    judicial_precedent_impact,
    'How would a definitive court ruling on a high-profile GPL linking case impact the stability and acceptance of the ''interface boundary'' reading?',
    'Observation of legal outcomes and subsequent industry practice following a landmark court decision in a major jurisdiction.',
    'A ruling supporting this reading would solidify its position, potentially reducing resistance and suppression. A ruling against it would significantly weaken it, increasing extraction for its proponents or forcing re-evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_precedent_impact, empirical, 'The potential for judicial precedent to alter the constraint''s structural properties.').

omega_variable(
    copyleft_intent_vs_modularity,
    'Is the ''interface boundary'' reading consistent with the original intent of copyleft to maximize software freedom, or does it prioritize modularity and commercial integration at the expense of that intent?',
    'Historical analysis of GPL''s founding documents and early interpretations, combined with a philosophical assessment of ''software freedom'' in contemporary modular ecosystems.',
    'If found inconsistent, the legitimacy of this reading would be challenged, increasing resistance and potentially shifting its classification towards a Snare. If found consistent, its legitimacy would be strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(copyleft_intent_vs_modularity, preference, 'Tension between the philosophical goals of copyleft and the practicalities of modular software development.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__interface_boundary_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t5, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gpl__tr_t10, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(gpl__tr_t15, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(gpl__tr_t20, gpl_derivative_work_trigger__interface_boundary_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__interface_boundary_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__interface_boundary_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
