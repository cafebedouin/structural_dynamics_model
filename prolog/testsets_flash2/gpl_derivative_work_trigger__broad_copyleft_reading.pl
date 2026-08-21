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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: GPL Broad Copyleft Reading: Linking Creates Derivative Work
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'broad copyleft' reading of the GNU
 *   General Public License (GPL), which asserts that linking (even
 *   dynamically) with GPL-licensed software creates a 'derivative work,'
 *   thereby triggering the GPL's reciprocal source code disclosure
 *   obligations. This interpretation is central to the 'viral' nature of
 *   copyleft, aiming to expand the free software commons. It is a specific
 *   reading of the broader 'gpl_derivative_work_trigger' kernel, which is
 *   highly contested in copyright law and software development.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.35).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.6).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Reading: Linking Creates Derivative Work").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '8d744524-3c70-4d6f-8a8f-d8225212ec47').
narrative_ontology:cs_kernel_codification('8d744524-3c70-4d6f-8a8f-d8225212ec47', fixed_text).
narrative_ontology:cs_authority_grounding('8d744524-3c70-4d6f-8a8f-d8225212ec47', lineage).
narrative_ontology:cs_interpretation_layer_present('8d744524-3c70-4d6f-8a8f-d8225212ec47').
narrative_ontology:cs_reading_relation('8d744524-3c70-4d6f-8a8f-d8225212ec47', gpl_derivative_work_trigger__narrow_linking_permissive_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d744524-3c70-4d6f-8a8f-d8225212ec47', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('8d744524-3c70-4d6f-8a8f-d8225212ec47', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('8d744524-3c70-4d6f-8a8f-d8225212ec47', linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('8d744524-3c70-4d6f-8a8f-d8225212ec47', secondary, copyleft_expands_commons).
narrative_ontology:cs_axiom_status(copyleft_expands_commons, holdable).
narrative_ontology:cs_axiom_grounding('8d744524-3c70-4d6f-8a8f-d8225212ec47', copyleft_expands_commons, instrumental).
narrative_ontology:cs_reference_frame('8d744524-3c70-4d6f-8a8f-d8225212ec47', original_gpl_intent).
narrative_ontology:cs_drift_state('8d744524-3c70-4d6f-8a8f-d8225212ec47', contemporary_software_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d744524-3c70-4d6f-8a8f-d8225212ec47', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_software_users).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, copyright_lawyers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the assurance that software linked with GPL components will remain open, ensuring access to source code and the freedom to modify and distribute. This reading expands their rights.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_software_users, beneficiary,
    organized, generational, mobile, global).

% Advocates for and enforces the broad interpretation of 'derivative work' to maximize the 'viral' effect of copyleft, expanding the commons. They actively monitor compliance and initiate legal action.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_community, agenda_setter,
    organized, generational, mobile, global).

% Face significant compliance costs or re-architecting efforts to avoid linking with GPL components under this broad interpretation, as it would require them to open-source their proprietary code. Their options are to comply, avoid GPL, or litigate.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Bear the cost of ensuring their integrated solutions do not inadvertently trigger GPL obligations, often requiring careful legal review and technical isolation of components. This adds complexity and cost to their development process.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Benefit from the increased demand for legal advice, compliance audits, and litigation services stemming from the ambiguities and enforcement actions related to GPL interpretation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, copyright_lawyers, beneficiary,
    institutional, biographical, arbitrage, national).

% As creators, they must navigate complex licensing decisions. They benefit when their own GPL-licensed code is protected by this reading, but pay a cost in restricted choices when integrating other components into proprietary projects.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, software_developers, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expansion of the software commons by ensuring that code linked with GPL components also becomes open source, fostering collaborative development and preventing proprietary enclosure of shared work.
% TRANSFER_FUNCTION: Transfers proprietary rights and source code access from proprietary software vendors and commercial integrators to GPL software users and the open-source community, in exchange for using GPL-licensed components.
% ABSENT_VOICES: Proprietary software advocates and some commercial entities who argue for a more permissive interpretation of 'derivative work' are often excluded from the core discussions within the open-source community, where this broad reading is championed.
% DISAPPEARANCE_RATIONALE: If this broad reading of GPL vanished, proprietary vendors would freely link with GPL code without fear of source disclosure, leading to a significant reduction in the 'viral' effect of copyleft. The open-source ecosystem would shrink, and proprietary software would absorb many currently open components, fundamentally altering the software landscape.
% FOUNDING_PROBLEM: Proprietary software vendors were enclosing and privatizing improvements to publicly shared software, undermining the collaborative spirit and preventing users from having full control over their software.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and other open-source advocates consistently attest that the problem of proprietary enclosure remains live, justifying the continued enforcement of strong copyleft. Independent legal scholars and some developers corroborate the ongoing tension between open and proprietary models.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.35) is moderate, representing the cost to proprietary vendors who must either open-source their code or re-architect to avoid GPL components. Suppression (0.6) is significant, as it requires active legal enforcement and community vigilance to ensure compliance and deter non-compliance. The theater ratio is low (0.1) because the enforcement actions are genuine and directly tied to the goal of expanding the commons. The claimed type is 'rope' because, from the perspective of the open-source community, it is a coordination mechanism to ensure shared freedom, even if it extracts from proprietary interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the open-source community, this is a necessary coordination mechanism to protect and expand the commons. From the perspective of proprietary vendors, it is an extractive snare that forces them to give up intellectual property. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The open-source community and GPL software users are beneficiaries, gaining expanded access to source code and fostering a collaborative ecosystem. Proprietary software vendors and commercial integrators are payers, bearing the costs of compliance or avoidance. Copyright lawyers benefit from the increased legal complexity. Software developers have a dual role, benefiting when their own GPL code is protected but paying when integrating GPL into proprietary projects.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_precedent_stability,
    'How stable is the legal precedent supporting this broad interpretation of ''derivative work'' in various jurisdictions?',
    'Analysis of court rulings and legislative changes in key jurisdictions (e.g., US, EU, Germany) over time. A consistent pattern of upholding this reading would increase confidence.',
    'If precedent is weak or inconsistent, the effective suppression and extractiveness of this reading would be lower, as proprietary vendors would have more avenues for legal challenge or avoidance. This could shift the classification towards a more permissive ''rope'' or even ''piton'' if enforcement becomes purely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_precedent_stability, empirical, 'The legal enforceability of the broad copyleft interpretation.').

omega_variable(
    technological_coupling_ambiguity,
    'At what level of technological coupling (e.g., static vs. dynamic linking, IPC, plugins) does ''derivation'' unambiguously occur, and how does this reading map to evolving software architectures?',
    'Expert consensus from software architects and legal scholars on specific technical scenarios, potentially codified in updated licensing guidance or case law. This would clarify the boundary conditions for ''derivative work''.',
    'If the technological boundary for ''derivative work'' is clarified and narrowed, this reading''s scope would shrink, reducing its extractiveness and suppression on proprietary code. If it''s broadened to new forms of coupling, its impact would increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_coupling_ambiguity, conceptual, 'Clarity of ''derivative work'' definition in evolving technical contexts.').

omega_variable(
    alternative_licensing_impact,
    'To what extent do alternative permissive licenses (e.g., MIT, Apache) reduce the perceived necessity or impact of this broad copyleft reading?',
    'Market share analysis of open-source projects using permissive vs. copyleft licenses, and surveys of developer preferences and compliance strategies. This would indicate if developers are ''voting with their feet'' for less restrictive options.',
    'If permissive licenses become dominant, the broad copyleft reading might be seen as less essential for the health of the commons, potentially reducing its perceived coordination function and increasing its perceived extraction. This could shift its classification towards a ''tangled_rope'' or ''snare'' if its benefits are seen as outweighed by its costs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_licensing_impact, preference, 'Impact of permissive licenses on the perceived value of broad copyleft.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 1989, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(gpl__be_t1989, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1989, 0.2).
narrative_ontology:measurement(gpl__be_t1999, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1999, 0.3).
narrative_ontology:measurement(gpl__be_t2009, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(gpl__be_t2019, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2019, 0.35).
narrative_ontology:measurement(gpl__be_t2024, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1989, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1989, 0.4).
narrative_ontology:measurement(gpl__su_t1999, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1999, 0.5).
narrative_ontology:measurement(gpl__su_t2009, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2009, 0.6).
narrative_ontology:measurement(gpl__su_t2019, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(gpl__su_t2024, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_derivative_work_trigger' kernel. This 'broad_copyleft_reading' asserts that linking (even dynamic) creates a derivative work, triggering GPL obligations. It is linked to the 'narrow_linking_permissive_reading' (linking is aggregation, not derivation) and the 'interface_boundary_reading' (clean API boundaries constitute non-derivative aggregation), which represent competing interpretations of the same core legal concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
