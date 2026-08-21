% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant for Shared Infrastructure & Externalities (Coordination Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coordination reading' of HOA
 *   covenants, where the primary function is to facilitate shared
 *   infrastructure maintenance and resolve genuine negative externalities
 *   within a community. In this reading, the covenant operates as a Rope,
 *   providing collective benefits with minimal extraction, primarily
 *   targeting free-riders. The metrics reflect a low extractiveness and
 *   suppression, consistent with a genuine coordination mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.18).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.25).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant for Shared Infrastructure & Externalities (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '055abc68-6368-4057-a99e-7ee8b9aabdb0').
narrative_ontology:cs_kernel_codification('055abc68-6368-4057-a99e-7ee8b9aabdb0', formalized).
narrative_ontology:cs_authority_grounding('055abc68-6368-4057-a99e-7ee8b9aabdb0', practice).
narrative_ontology:cs_interpretation_layer_present('055abc68-6368-4057-a99e-7ee8b9aabdb0').
narrative_ontology:cs_reading_relation('055abc68-6368-4057-a99e-7ee8b9aabdb0', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('055abc68-6368-4057-a99e-7ee8b9aabdb0', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('055abc68-6368-4057-a99e-7ee8b9aabdb0', foundational, collective_benefit_justifies_contribution).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_contribution, holdable).
narrative_ontology:cs_axiom_grounding('055abc68-6368-4057-a99e-7ee8b9aabdb0', collective_benefit_justifies_contribution, deontological).
narrative_ontology:cs_axiom('055abc68-6368-4057-a99e-7ee8b9aabdb0', foundational, objective_externalities_require_regulation).
narrative_ontology:cs_axiom_status(objective_externalities_require_regulation, holdable).
narrative_ontology:cs_axiom_grounding('055abc68-6368-4057-a99e-7ee8b9aabdb0', objective_externalities_require_regulation, empirically_contingent).
narrative_ontology:cs_reference_frame('055abc68-6368-4057-a99e-7ee8b9aabdb0', community_self_governance_for_common_good).
narrative_ontology:cs_drift_state('055abc68-6368-4057-a99e-7ee8b9aabdb0', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('055abc68-6368-4057-a99e-7ee8b9aabdb0', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from well-maintained common areas (parks, roads, utilities) and protection against genuine nuisances that would degrade property values. They pay regular assessments for these services. Exit involves selling their home, which is constrained by market conditions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).

% Administers the covenant, collects assessments, and contracts for maintenance. Enforces rules related to shared infrastructure and objective externalities (e.g., unkempt lawns attracting pests, excessive noise). Their power is derived from the homeowners' collective agreement.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, constrained, local).

% Homeowners who attempt to avoid paying assessments or neglect their property in ways that create objective nuisances for neighbors. They are subject to fines and liens if they do not comply, with limited options to avoid these costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, trapped, local).

% Monitors HOA operations for compliance with state and local laws, particularly regarding financial transparency and due process. Can intervene in disputes but generally defers to the HOA's self-governance.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, local_government, observer,
    institutional, generational, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective funding and maintenance of shared infrastructure (e.g., private roads, common landscaping, shared utilities) and provides a mechanism for resolving genuine negative externalities (e.g., unmanaged blight, excessive noise) that affect property values and quality of life for all residents.
% TRANSFER_FUNCTION: Collects regular assessments from all homeowners to fund shared infrastructure maintenance and transfers these funds to contractors or service providers. It also transfers the cost of non-compliance (fines) from rule-breakers to the HOA for general use.
% ABSENT_VOICES: Homeowners who feel the covenant's scope is being expanded beyond its original intent for coordination into behavioral control or extraction; they are often outvoted or lack the resources to challenge the board effectively.
% DISAPPEARANCE_RATIONALE: If the covenant vanished, shared infrastructure would quickly degrade due to lack of funding and coordination. Externalities would proliferate, leading to a decline in property values and quality of life, necessitating a new collective governance mechanism or increased local government intervention.
% FOUNDING_PROBLEM: To ensure the long-term maintenance of shared community assets and to prevent individual actions from negatively impacting collective property values and living standards, which a purely individual ownership model could not address.
% FOUNDING_PROBLEM_CORROBORATION: Local government planning departments and real estate developers corroborate the ongoing need for such mechanisms in planned communities to ensure long-term viability and prevent 'tragedy of the commons' scenarios. Homeowners themselves, when surveyed, generally agree on the necessity of basic infrastructure maintenance.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.18, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because assessments are directly tied to the cost of shared services, and fines are for objective, measurable harms. Suppression is also low (0.25) as enforcement is limited to ensuring participation in collective goods and preventing clear nuisances, with reasonable due process. Theater ratio is negligible (0.05) as the covenant's stated purpose aligns closely with its actual operation. The slight increase in extractiveness and suppression over time reflects minor administrative overhead growth and the need to address occasional non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_homeowners' and the 'hoa_board' (in this reading), the covenant is a clear Rope, providing essential coordination. From the perspective of 'free_riders', it is a coercive mechanism, but one that is justified by the collective good. This reading explicitly excludes the perspectives that see the covenant as primarily a tool for aesthetic control or revenue generation.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are beneficiaries, as they collectively gain from maintained infrastructure and a stable living environment. The HOA board acts as the agenda-setter, administering the agreed-upon rules. Free-riders are the victims, as they are compelled to contribute to the collective good they would otherwise exploit. The system is designed for symmetric benefit, with costs primarily borne by those who deviate from the collective agreement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_creep_potential,
    'Does the covenant''s enforcement scope remain strictly limited to shared infrastructure and objective externalities, or is there a risk of ''scope creep'' into subjective behavioral control?',
    'Longitudinal analysis of enforcement actions: track the proportion of enforcement actions related to objective infrastructure/nuisance vs. subjective aesthetic/behavioral rules over time. Legal challenges to specific rules could also clarify boundaries.',
    'If scope creep is significant, the constraint would shift towards a ''behavioral_control_reading'' (higher extractiveness, higher suppression, potentially a Tangled Rope or Snare), as it would be enforcing subjective preferences rather than objective collective goods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_potential, empirical, 'Risk of the covenant''s scope expanding beyond its coordination function.').

omega_variable(
    assessment_cost_transparency,
    'Are the HOA assessments genuinely tied to the actual costs of shared infrastructure maintenance and externality resolution, or do they include hidden administrative overhead or discretionary spending?',
    'Independent audit of HOA financials, comparing assessment revenue to itemized maintenance and operational costs. Benchmarking against similar communities'' costs.',
    'If assessments significantly exceed actual costs, the constraint''s extractiveness would be higher, pushing it towards a ''tangled_rope'' or ''extraction_reading'', as it would be collecting rents beyond necessary coordination costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assessment_cost_transparency, empirical, 'Transparency and justification of HOA assessment costs.').

omega_variable(
    reading_framing_ambiguity,
    'Is this ''coordination reading'' the dominant interpretation among homeowners and the board, or is it a normative ideal that masks underlying behavioral control or extraction motives?',
    'Surveys of homeowners and board members regarding their primary motivations for covenant adherence and enforcement. Analysis of board meeting minutes and community communications for framing language.',
    'If the ''coordination reading'' is not genuinely dominant, the constraint''s effective classification would be closer to the ''behavioral_control_reading'' or ''extraction_reading'', indicating a disconnect between claimed purpose and actual function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Dominance of the coordination framing versus other interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__coordination_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__coordination_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__coordination_reading, base_extractiveness, 15, 0.18).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.23).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__coordination_reading, suppression_requirement, 15, 0.24).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hoa_covenant_scope' kernel. This 'coordination_reading' focuses on shared infrastructure and externalities, while 'behavioral_control_reading' and 'extraction_reading' represent alternative interpretations of the covenant's primary function and impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
