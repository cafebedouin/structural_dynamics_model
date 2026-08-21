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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: GPL Derivative Work Trigger: Narrow Linking Permissive Reading
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents a permissive interpretation of the GNU General
 *   Public License (GPL) regarding what constitutes a 'derivative work' when
 *   linking proprietary software with GPL-licensed libraries. Under this
 *   reading, linking is considered aggregation, not derivation, meaning that
 *   proprietary modules can incorporate GPL code without triggering the GPL's
 *   copyleft obligations to release their own source. This creates a 'wall'
 *   protecting proprietary modules, frustrating the GPL's goal of propagating
 *   free software. The claimed type is 'tangled_rope' because it facilitates
 *   coordination (code reuse) but with significant asymmetric extraction
 *   (proprietary vendors benefit at the expense of GPL users and the
 *   open-source community).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.65).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.7).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "GPL Derivative Work Trigger: Narrow Linking Permissive Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '4696215f-1fc6-4664-9256-b7c9c01d04a6').
narrative_ontology:cs_kernel_codification('4696215f-1fc6-4664-9256-b7c9c01d04a6', fixed_text).
narrative_ontology:cs_authority_grounding('4696215f-1fc6-4664-9256-b7c9c01d04a6', practice).
narrative_ontology:cs_interpretation_layer_present('4696215f-1fc6-4664-9256-b7c9c01d04a6').
narrative_ontology:cs_reading_relation('4696215f-1fc6-4664-9256-b7c9c01d04a6', gpl_derivative_work_trigger__broad_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('4696215f-1fc6-4664-9256-b7c9c01d04a6', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('4696215f-1fc6-4664-9256-b7c9c01d04a6', foundational, linking_is_aggregation_not_derivation).
narrative_ontology:cs_axiom_status(linking_is_aggregation_not_derivation, holdable).
narrative_ontology:cs_axiom_grounding('4696215f-1fc6-4664-9256-b7c9c01d04a6', linking_is_aggregation_not_derivation, conventional).
narrative_ontology:cs_axiom('4696215f-1fc6-4664-9256-b7c9c01d04a6', foundational, gpl_obligations_only_on_modification).
narrative_ontology:cs_axiom_status(gpl_obligations_only_on_modification, holdable).
narrative_ontology:cs_axiom_grounding('4696215f-1fc6-4664-9256-b7c9c01d04a6', gpl_obligations_only_on_modification, conventional).
narrative_ontology:cs_reference_frame('4696215f-1fc6-4664-9256-b7c9c01d04a6', commercial_software_interoperability).
narrative_ontology:cs_drift_state('4696215f-1fc6-4664-9256-b7c9c01d04a6', contemporary_legal_disputes, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4696215f-1fc6-4664-9256-b7c9c01d04a6', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_software_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from being able to link proprietary code with GPL libraries without triggering copyleft obligations, allowing them to leverage open-source components while retaining control over their intellectual property. This reading provides legal cover for their business model.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Utilizes GPL-licensed code in larger, proprietary applications, interpreting linking as a form of aggregation that does not create a derivative work. This allows them to avoid releasing their own source code, preserving competitive advantage.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, commercial_developers, beneficiary,
    powerful, biographical, mobile, global).

% Loses the full source-availability guarantee for software that incorporates GPL components via linking, as proprietary modules remain closed. This frustrates their right to inspect, modify, and share the complete software system.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_software_users, payer,
    moderate, biographical, constrained, global).

% Sees the propagation goal of the GPL frustrated, as this reading allows proprietary software to 'wall off' GPL code, preventing the expansion of the free software ecosystem. They bear the cost of reduced freedom and transparency.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, open_source_community, payer,
    organized, generational, constrained, global).

% The primary author and enforcer of the GPL, advocating for the broad copyleft interpretation. This reading directly challenges their core mission and legal interpretations, forcing them into continuous defense and litigation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, free_software_foundation, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Interpret and adjudicate disputes arising from GPL licensing. Their rulings can either reinforce or undermine this permissive reading, shaping the legal landscape for software development and distribution.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, courts_and_legal_scholars, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows diverse software components, including GPL-licensed libraries, to be combined into larger applications, facilitating interoperability and code reuse across different licensing regimes.
% TRANSFER_FUNCTION: Transfers the benefit of using GPL-licensed code to proprietary software vendors and commercial developers without the reciprocal obligation of sharing their own source code, effectively privatizing the commons.
% ABSENT_VOICES: Users who expect full source code availability for any software incorporating GPL components are often not directly represented in the legal interpretations that favor this permissive reading. Their expectation of freedom is suppressed by the legal framing.
% DISAPPEARANCE_RATIONALE: If this permissive reading vanished, proprietary software linking to GPL code would immediately face copyleft obligations, forcing many vendors to either re-license their products, re-architect their software, or cease using GPL components. This would significantly alter the software industry's landscape and business models.
% FOUNDING_PROBLEM: The need for clear legal boundaries around what constitutes a 'derivative work' when combining software components under different licenses, particularly to enable commercial use of open-source code without stifling innovation.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software vendors and commercial developers attest that the problem of legal clarity for linking remains live, arguing that overly broad interpretations of 'derivative work' hinder innovation. The Free Software Foundation, however, argues that the problem is not one of clarity but of compliance with the GPL's intended scope, and that this reading undermines the original intent.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high because proprietary vendors gain substantial value from using GPL code without reciprocal obligations, effectively privatizing a common resource. Suppression is also high because this reading actively suppresses the GPL's intended propagation mechanism, requiring continuous legal defense by the FSF and limiting user freedoms. The low theater ratio reflects that the legal interpretation, while contested, is genuinely applied and enforced in practice, not merely performative. Accessibility collapse is moderate as alternative licenses exist, but the market dominance of proprietary platforms makes avoiding GPL components difficult for many developers. Resistance is moderate as the FSF and open-source community actively challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries (proprietary vendors) perceive this as a legitimate interpretation that fosters innovation and interoperability, a 'rope' that enables efficient software development. The victims (GPL users, open-source community) perceive it as a 'snare' that undermines the core principles of free software and extracts value by circumventing copyleft. The engine's classification as 'tangled_rope' reflects this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary software vendors and commercial developers are clear beneficiaries, leveraging GPL code without full compliance. GPL software users and the open-source community are victims, losing the source-availability guarantee and seeing the GPL's propagation goal frustrated. The Free Software Foundation acts as an agenda-setter, constantly defending its interpretation against this permissive reading. Courts and legal scholars are observers, adjudicating the ongoing contest.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_precedent_stability,
    'How stable is the legal precedent supporting this narrow linking interpretation across different jurisdictions and over time?',
    'Analysis of court rulings and legislative changes in major software markets; tracking of successful and unsuccessful GPL enforcement actions.',
    'If precedent is unstable or shifts towards broader copyleft, the extractiveness and suppression of this reading would decrease, potentially reclassifying it towards a ''rope'' or even ''mountain'' (if the broader reading becomes universally accepted). If it hardens, it entrenches the ''tangled_rope'' or ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_precedent_stability, empirical, 'The legal robustness of the narrow linking interpretation.').

omega_variable(
    technical_coupling_definition,
    'What level of technical coupling (e.g., static vs. dynamic linking, shared memory, IPC) definitively constitutes a ''derivative work'' versus ''aggregation''?',
    'Expert consensus from software architects and legal scholars, potentially codified in updated licensing guidelines or judicial interpretations that provide clear technical criteria.',
    'Clearer technical definitions could reduce ambiguity, potentially lowering suppression and extractiveness if compliance becomes more straightforward, or increasing it if the definition favors broader copyleft. This could shift the classification towards a ''rope'' (if clear rules enable fair coordination) or a ''snare'' (if rules are used to enforce extraction more effectively).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technical_coupling_definition, conceptual, 'The technical boundary between derivative work and aggregation.').

omega_variable(
    gpl_propagation_effectiveness,
    'To what extent does this permissive reading actually frustrate the GPL''s goal of propagating free software, versus simply enabling wider adoption of GPL components?',
    'Empirical studies tracking the growth of proprietary software incorporating GPL components versus the growth of purely free software ecosystems; surveys of developer motivations and licensing choices.',
    'If the permissive reading is found to significantly hinder propagation, its ''snare'' characteristics are amplified. If it''s found to enable net positive growth for the free software ecosystem (even if indirectly), its ''rope'' characteristics are emphasized, potentially lowering its effective extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gpl_propagation_effectiveness, empirical, 'Impact of permissive linking on GPL''s propagation goal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(gpl__be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(gpl__be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gpl__be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(gpl__be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(gpl__be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl__su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(gpl__su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(gpl__su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.67).
narrative_ontology:measurement(gpl__su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(gpl__su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__narrow_linking_permissive_reading, information_standard).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.05).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_licensing_models).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel. This 'narrow_linking_permissive_reading' directly influences the legal and practical landscape for the 'broad_copyleft_reading' and 'interface_boundary_reading' by providing a competing interpretation that enables proprietary integration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
