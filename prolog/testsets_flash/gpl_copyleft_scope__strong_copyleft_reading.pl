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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_copyleft_scope__strong_copyleft_reading
 *   human_readable: GPL Strong Copyleft Scope (Dynamic Linking)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the 'strong copyleft' reading of GPL Section
 *   2(b), which asserts that any work combined with or dynamically linked to
 *   GPL-licensed code must also be GPL-licensed. This interpretation extends
 *   the 'derivative work' boundary broadly to ensure the continued freedom of
 *   software. It functions as a snare for proprietary vendors who wish to
 *   leverage GPL components without releasing their own source code, while
 *   acting as a strong coordination mechanism for the free software
 *   community. The high extractiveness reflects the commercial cost imposed
 *   on proprietary actors, and high suppression reflects the active
 *   enforcement required to maintain this boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__strong_copyleft_reading, 0.85).
domain_priors:suppression_score(gpl_copyleft_scope__strong_copyleft_reading, 0.75).
domain_priors:theater_ratio(gpl_copyleft_scope__strong_copyleft_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gpl_copyleft_scope__strong_copyleft_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__strong_copyleft_reading, snare).
narrative_ontology:human_readable(gpl_copyleft_scope__strong_copyleft_reading, "GPL Strong Copyleft Scope (Dynamic Linking)").
narrative_ontology:topic_domain(gpl_copyleft_scope__strong_copyleft_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__strong_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__strong_copyleft_reading, 'ce3a078b-19b4-49ca-aff4-3616218fcf42').
narrative_ontology:cs_kernel_codification('ce3a078b-19b4-49ca-aff4-3616218fcf42', fixed_text).
narrative_ontology:cs_authority_grounding('ce3a078b-19b4-49ca-aff4-3616218fcf42', lineage).
narrative_ontology:cs_interpretation_layer_present('ce3a078b-19b4-49ca-aff4-3616218fcf42').
narrative_ontology:cs_reading_relation('ce3a078b-19b4-49ca-aff4-3616218fcf42', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce3a078b-19b4-49ca-aff4-3616218fcf42', gpl_copyleft_scope__enforcement_vacuum_reading, coexists_with).
narrative_ontology:cs_axiom('ce3a078b-19b4-49ca-aff4-3616218fcf42', foundational, copyleft_extends_to_all_coupling).
narrative_ontology:cs_axiom_status(copyleft_extends_to_all_coupling, holdable).
narrative_ontology:cs_axiom_grounding('ce3a078b-19b4-49ca-aff4-3616218fcf42', copyleft_extends_to_all_coupling, conventional).
narrative_ontology:cs_axiom('ce3a078b-19b4-49ca-aff4-3616218fcf42', secondary, user_freedom_requires_source_availability).
narrative_ontology:cs_axiom_status(user_freedom_requires_source_availability, holdable).
narrative_ontology:cs_axiom_grounding('ce3a078b-19b4-49ca-aff4-3616218fcf42', user_freedom_requires_source_availability, deontological).
narrative_ontology:cs_reference_frame('ce3a078b-19b4-49ca-aff4-3616218fcf42', gpl_v2_original_intent).
narrative_ontology:cs_drift_state('ce3a078b-19b4-49ca-aff4-3616218fcf42', contemporary_software_ecosystem, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ce3a078b-19b4-49ca-aff4-3616218fcf42', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, gpl_developers).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__strong_copyleft_reading, free_software_users).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary author and enforcer of the GPL. Interprets Section 2(b) broadly to ensure that any software linked with GPL code, even dynamically, must also be GPL-licensed. Actively pursues enforcement actions and provides legal guidance to uphold this interpretation.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).

% Authors of GPL-licensed code who benefit from the strong copyleft interpretation, as it ensures their contributions remain free and open, preventing proprietary enclosure. They rely on the FSF's enforcement to protect their work's license terms.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, gpl_developers, beneficiary,
    organized, biographical, mobile, global).

% Users who benefit from the availability of source code and the freedom to modify and distribute software, which is guaranteed by the strong copyleft. They are indirect beneficiaries of the enforcement actions that keep code open.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, free_software_users, beneficiary,
    moderate, biographical, constrained, global).

% Companies that develop and sell proprietary software. They are victims of this strong copyleft interpretation because it forces them to either avoid GPL components entirely, or to release their entire codebase under the GPL if they link to GPL libraries, which is commercially unviable for them. They seek to interpret GPL more narrowly.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Businesses that integrate various software components, including open-source libraries, into larger commercial products. They face significant legal risk and compliance costs under the strong copyleft interpretation, as dynamic linking can trigger the GPL's requirements, forcing them to choose between GPL compliance or avoiding valuable components.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, commercial_integrators, payer,
    moderate, biographical, constrained, global).

% Academics and legal experts who analyze the implications of GPL licensing, the definition of 'derivative work,' and the enforceability of copyleft in various jurisdictions. They provide commentary and analysis but do not directly enforce or pay.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__strong_copyleft_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of free software by ensuring that contributions to GPL-licensed projects, and any works linked with them, remain free and open, fostering a shared commons of code.
% TRANSFER_FUNCTION: Transfers the right to restrict distribution and modification of combined or dynamically linked works from proprietary developers to the free software community, effectively requiring source code disclosure.
% ABSENT_VOICES: Proprietary software developers who wish to use GPL components without releasing their own source code are structurally excluded from the 'free software commons' as defined by this reading. They would argue for a more permissive interpretation of 'derivative work' to allow broader integration.
% DISAPPEARANCE_RATIONALE: If the strong copyleft interpretation vanished, proprietary vendors would immediately integrate GPL components without source release, leading to a rapid enclosure of previously 'free' code. The free software ecosystem would fragment, and the FSF's mission would be severely undermined.
% FOUNDING_PROBLEM: The original problem was the enclosure of software by proprietary interests, preventing users from studying, sharing, and modifying code, leading to a loss of user freedom and a fragmented software landscape.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and GPL developers consistently attest that the threat of proprietary enclosure remains live, necessitating strong copyleft. Independent legal scholars and open-source advocates corroborate that without such mechanisms, proprietary interests would indeed enclose free code, even if the specific mechanisms of enclosure evolve.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__strong_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__strong_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__strong_copyleft_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_copyleft_scope__strong_copyleft_reading, 'none', 1).

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
 *   Extractiveness is high because proprietary vendors face a stark choice: either fully GPL-license their product (a significant commercial cost) or avoid valuable GPL components. Suppression is high due to the active legal enforcement by the FSF and other GPL advocates against perceived violations, which effectively 'suppresses' alternative interpretations or integration strategies. Theater ratio is low because the enforcement actions are genuinely aimed at upholding the license's core intent, not merely performative. The increasing trend in extractiveness and suppression reflects the growing commercial value of software and the intensifying legal battles over open-source compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the FSF and GPL developers, this is a necessary 'Rope' for coordinating free software development and preventing enclosure. From the perspective of proprietary vendors, it is a 'Snare' that extracts commercial value by forcing unwanted license changes or component avoidance. The engine's classification will reflect the latter due to the high extractiveness and suppression against identifiable victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The Free Software Foundation and GPL developers are clear beneficiaries, as this reading directly supports their mission and ensures their code remains free (d near 0.0). Proprietary software vendors and commercial integrators are the primary victims, facing significant costs and restrictions (d near 1.0). Free software users are indirect beneficiaries, gaining access to a larger pool of free software. Legal scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_precedent_ambiguity,
    'Would a definitive judicial ruling on dynamic linking and derivative works uphold or narrow the strong copyleft interpretation?',
    'A landmark court case specifically addressing the ''linking'' debate under GPL Section 2(b).',
    'If a court upholds the strong copyleft, the constraint''s legitimacy and enforcement power would increase, potentially raising extractiveness. If it narrows the scope, extractiveness would decrease, and the constraint might shift towards a ''Rope'' or ''Piton'' for proprietary actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_precedent_ambiguity, empirical, 'Uncertainty regarding future judicial interpretation of GPL''s scope.').

omega_variable(
    technological_circumvention_risk,
    'Could new software architectures or linking technologies emerge that effectively circumvent the strong copyleft interpretation without violating its letter?',
    'Emergence and widespread adoption of new technical patterns (e.g., microservices, API-only interaction) that allow integration without ''linking'' in the traditional sense.',
    'Successful circumvention would reduce the constraint''s effective suppression and extractiveness, potentially shifting it towards a ''Piton'' as its enforcement becomes theatrical or obsolete.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_circumvention_risk, empirical, 'Risk of technological developments undermining copyleft enforcement.').

omega_variable(
    derivative_work_definition_conceptual,
    'Is the ''derivative work'' boundary a fixed legal concept or a flexible interpretation that adapts to technological context and policy goals?',
    'Conceptual analysis and legal philosophy debates on the nature of intellectual property and its application to software, particularly in the context of open-source licenses.',
    'If fixed, the strong copyleft reading is either correct or incorrect based on existing law. If flexible, its persistence depends on the ongoing policy debate and the power of its proponents to shape legal interpretation, making its ''naturalness'' more contestable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_definition_conceptual, conceptual, 'Conceptual ambiguity in the definition of ''derivative work'' in software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__strong_copyleft_reading, 1991, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl__tr_t2000, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gpl__tr_t2010, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(gpl__tr_t2024, gpl_copyleft_scope__strong_copyleft_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 1991, 0.7).
narrative_ontology:measurement(gpl__be_t2000, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(gpl__be_t2010, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(gpl__be_t2024, gpl_copyleft_scope__strong_copyleft_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 1991, 0.6).
narrative_ontology:measurement(gpl__su_t2000, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(gpl__su_t2010, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(gpl__su_t2024, gpl_copyleft_scope__strong_copyleft_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__strong_copyleft_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__narrow_scope_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, gpl_copyleft_scope__enforcement_vacuum_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__strong_copyleft_reading, proprietary_software_licensing_models).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_copyleft_scope' kernel. This 'strong_copyleft_reading' asserts a broad interpretation of derivative works, directly influencing the 'narrow_scope_reading' (by providing a counter-interpretation) and the 'enforcement_vacuum_reading' (by demonstrating active enforcement capacity). It also affects 'proprietary_software_licensing_models' by limiting their ability to integrate GPL components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
