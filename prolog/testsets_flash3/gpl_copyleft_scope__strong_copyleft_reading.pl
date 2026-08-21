% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__strong_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__strong_copyleft_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Scope (Dynamic Linking/Combined Works)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strong copyleft' reading of the GNU
 *   General Public License (GPL), specifically concerning Section 2(b) and
 *   the definition of a 'derivative work' to include combined or dynamically
 *   linked software. Under this reading, any software that links to or
 *   combines with GPL-licensed code must also be licensed under the GPL,
 *   effectively 'snaring' proprietary code into the free software ecosystem.
 *   This interpretation is actively enforced by the Free Software Foundation
 *   and aligned projects, creating a high-extraction environment for
 *   proprietary vendors who wish to integrate GPL components without
 *   releasing their own source code.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.85).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.75).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Scope (Dynamic Linking/Combined Works)").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, '9ec19988-faf9-4b25-9424-c252286d11b3').
narrative_ontology:cs_kernel_codification('9ec19988-faf9-4b25-9424-c252286d11b3', fixed_text).
narrative_ontology:cs_authority_grounding('9ec19988-faf9-4b25-9424-c252286d11b3', lineage).
narrative_ontology:cs_interpretation_layer_present('9ec19988-faf9-4b25-9424-c252286d11b3').
narrative_ontology:cs_reading_relation('9ec19988-faf9-4b25-9424-c252286d11b3', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ec19988-faf9-4b25-9424-c252286d11b3', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('9ec19988-faf9-4b25-9424-c252286d11b3', foundational, all_code_coupling_is_derivative).
narrative_ontology:cs_axiom_status(all_code_coupling_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('9ec19988-faf9-4b25-9424-c252286d11b3', all_code_coupling_is_derivative, conventional).
narrative_ontology:cs_axiom('9ec19988-faf9-4b25-9424-c252286d11b3', foundational, freedom_requires_reciprocity).
narrative_ontology:cs_axiom_status(freedom_requires_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('9ec19988-faf9-4b25-9424-c252286d11b3', freedom_requires_reciprocity, deontological).
narrative_ontology:cs_reference_frame('9ec19988-faf9-4b25-9424-c252286d11b3', gpl_v2_original_intent).
narrative_ontology:cs_drift_state('9ec19988-faf9-4b25-9424-c252286d11b3', contemporary_software_ecosystem, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ec19988-faf9-4b25-9424-c252286d11b3', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_developers).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary author and enforcer of the GPL. Interprets the license broadly to ensure maximum code freedom and prevent proprietary enclosure. Actively monitors for violations and initiates enforcement actions, particularly concerning dynamic linking and combined works. Benefits from the expansion of the free software ecosystem.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).

% Projects whose code is licensed under the GPL. They benefit from the strong copyleft ensuring that any derivative work or combined work remains free, preventing proprietary forks or integrations that would privatize their contributions. Their code base grows and remains open.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_licensed_projects, beneficiary,
    organized, generational, mobile, global).

% Individual developers who contribute to or use GPL-licensed software. They benefit from the assurance that their contributions will remain free and that they can always access and modify the full source code of any combined work. Their work is protected from proprietary appropriation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_developers, beneficiary,
    moderate, biographical, constrained, global).

% Companies that develop and sell proprietary software. Under this reading, they are forced to either avoid GPL-licensed components entirely, or to release their entire combined work under the GPL, which is antithetical to their business model. They bear the cost of either re-engineering or losing market opportunities.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Businesses that integrate various software components, including open-source, into larger commercial products. This reading forces them to consider the entire integrated product as a derivative work, requiring full GPL compliance for their proprietary components, which is a significant business risk and cost.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    organized, biographical, constrained, global).

% Academics and legal professionals who analyze software licensing and intellectual property law. They study the implications of the GPL's copyleft provisions, the legal enforceability of its scope, and the ongoing debates around derivative works and linking. Their analysis informs policy and judicial interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of free software by ensuring that all contributions and derivative works remain open and accessible, fostering a collaborative ecosystem where code is shared and improved upon without proprietary enclosure.
% TRANSFER_FUNCTION: Transfers the right to use, modify, and distribute source code from proprietary entities to the public domain (under GPL terms) whenever GPL-licensed components are combined or dynamically linked into a larger work. This effectively transfers potential proprietary value into the commons.
% ABSENT_VOICES: Proprietary software developers who wish to use GPL components without releasing their own source code are structurally excluded from this interpretation. They would argue for a narrower definition of 'derivative work' to protect their intellectual property, but their arguments are dismissed by the strong copyleft interpretation.
% DISAPPEARANCE_RATIONALE: If this strong interpretation of GPL copyleft vanished, proprietary vendors would immediately integrate GPL components into their closed-source products without fear of enforcement. This would lead to a significant enclosure of previously free code, fundamentally altering the free software ecosystem and diminishing the leverage of free software communities.
% FOUNDING_PROBLEM: The problem of proprietary software vendors taking open-source code, modifying it, and then distributing it as closed-source, thereby privatizing community contributions and preventing further collaborative development.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and numerous free software projects attest that proprietary enclosure remains a live and constant threat. Independent legal analysis and historical examples of proprietary forks corroborate the ongoing need for strong copyleft to protect the commons.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__strong_copyleft_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_copyleft_scope__strong_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because proprietary vendors are forced to either release their entire codebase under GPL (a significant 'cost' to their business model) or forgo using valuable GPL components. Suppression is also high (0.75) due to the active legal enforcement by the FSF and the structural impossibility of using GPL code in a proprietary product under this interpretation without triggering the copyleft. Theater ratio is low (0.1) because the enforcement is genuine and directly tied to the license's core function, not performative. The metrics reflect a robust, actively defended, and highly extractive interpretation of the license.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Free Software Foundation and free software developers, this is a necessary 'rope' for coordinating a free software commons and preventing enclosure. From the perspective of proprietary vendors, it is a 'snare' that extracts their intellectual property or forces them to abandon valuable components. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Free Software Foundation and GPL-licensed projects are clear beneficiaries, as this reading maximizes the reach and protection of free software. Proprietary software vendors and commercial integrators are the primary victims, facing significant costs or restrictions on their business models. Free software developers also benefit from the expanded commons. The strong copyleft acts as a 'snare' for proprietary interests, forcing them to contribute to the free software ecosystem or remain outside it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_precedent_ambiguity,
    'To what extent would a definitive judicial ruling on dynamic linking and derivative works alter the effective scope and enforceability of this strong copyleft reading?',
    'A landmark court case specifically addressing the ''linking'' debate and its implications for GPL Section 2(b).',
    'A ruling affirming the strong copyleft would solidify this reading, potentially increasing its extractiveness and suppression. A ruling favoring a narrower interpretation would weaken this reading, reducing its effective scope and potentially reclassifying it towards a ''piton'' or ''tangled_rope'' if enforcement becomes theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_precedent_ambiguity, empirical, 'Uncertainty regarding the legal precedent for GPL''s scope.').

omega_variable(
    technological_circumvention_risk,
    'How effectively can proprietary vendors develop technological workarounds (e.g., inter-process communication, APIs) to use GPL components without triggering the strong copyleft, and what is the cost of such circumvention?',
    'Empirical study of industry practices and legal challenges to specific circumvention techniques over time.',
    'If circumvention is cheap and effective, the constraint''s actual suppression and extractiveness would be lower than measured, as victims have a viable exit. If costly or ineffective, the current high extractiveness and suppression are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_circumvention_risk, empirical, 'Risk of technological workarounds reducing copyleft effectiveness.').

omega_variable(
    interpretive_community_cohesion,
    'How cohesive and unified is the free software community in consistently applying and enforcing this strong copyleft reading, particularly in projects not directly managed by the FSF?',
    'Analysis of licensing practices, community discussions, and enforcement actions across a broad range of GPL-licensed projects.',
    'If cohesion is high, the constraint''s enforcement and perceived extractiveness remain strong. If cohesion is low, the constraint may operate more like an ''enforcement_vacuum_reading'' in practice, with variable extractiveness depending on the specific project''s community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_community_cohesion, empirical, 'Cohesion of the free software community in enforcing strong copyleft.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t5, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t15, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gpl__tr_t25, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 25, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(gpl__be_t5, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(gpl__be_t10, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(gpl__be_t15, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 15, 0.83).
narrative_ontology:measurement(gpl__be_t20, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 20, 0.84).
narrative_ontology:measurement(gpl__be_t25, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 25, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(gpl__su_t5, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(gpl__su_t10, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(gpl__su_t15, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(gpl__su_t20, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(gpl__su_t25, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 25, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'strong_copyleft_reading' asserts a broad interpretation of derivative works, directly influencing the 'narrow_scope_reading' by providing a counter-interpretation, and the 'enforcement_vacuum_reading' by defining one pole of the interpretive contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
