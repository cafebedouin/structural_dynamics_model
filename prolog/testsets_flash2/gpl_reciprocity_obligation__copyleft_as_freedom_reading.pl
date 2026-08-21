% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the GPL's reciprocity obligation as a
 *   mechanism to preserve user freedoms by preventing proprietary capture.
 *   From this 'copyleft as freedom' reading, the 'viral' nature of the
 *   license is not a bug but a feature, actively suppressing proprietary
 *   business models that would otherwise enclose free software. The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinates
 *   a vast ecosystem of free software development while simultaneously
 *   extracting a 'cost' (the obligation to open-source derivatives) from
 *   proprietary integrators.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.78).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'c9fd4298-71f6-4604-811a-7001ad187289').
narrative_ontology:cs_kernel_codification('c9fd4298-71f6-4604-811a-7001ad187289', fixed_text).
narrative_ontology:cs_authority_grounding('c9fd4298-71f6-4604-811a-7001ad187289', lineage).
narrative_ontology:cs_interpretation_layer_present('c9fd4298-71f6-4604-811a-7001ad187289').
narrative_ontology:cs_reading_relation('c9fd4298-71f6-4604-811a-7001ad187289', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9fd4298-71f6-4604-811a-7001ad187289', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('c9fd4298-71f6-4604-811a-7001ad187289', foundational, user_freedom_is_paramount).
narrative_ontology:cs_axiom_status(user_freedom_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c9fd4298-71f6-4604-811a-7001ad187289', user_freedom_is_paramount, deontological).
narrative_ontology:cs_axiom('c9fd4298-71f6-4604-811a-7001ad187289', foundational, proprietary_capture_is_a_threat).
narrative_ontology:cs_axiom_status(proprietary_capture_is_a_threat, holdable).
narrative_ontology:cs_axiom_grounding('c9fd4298-71f6-4604-811a-7001ad187289', proprietary_capture_is_a_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('c9fd4298-71f6-4604-811a-7001ad187289', free_software_principles_of_user_liberty).
narrative_ontology:cs_drift_state('c9fd4298-71f6-4604-811a-7001ad187289', contemporary_cloud_computing_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c9fd4298-71f6-4604-811a-7001ad187289', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_developers_seeking_proprietary_derivatives).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes and defends the GPL, viewing it as essential for user freedom. They enforce the license through legal action and community pressure, ensuring derivatives remain open. They benefit from the growth of the free software ecosystem.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community, agenda_setter,
    organized, generational, constrained, global).

% Benefit from the assurance that software licensed under GPL will remain free and modifiable, preventing vendor lock-in and ensuring access to source code. They are protected from proprietary capture of essential tools.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Are compelled to release their modifications to GPL-licensed software under the same license if they distribute it. This restricts their ability to create proprietary derivative works and monetize them exclusively, imposing a 'viral' cost on their business models.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, immediate, constrained, global).

% Face a choice: either avoid GPL-licensed components or accept the reciprocity obligation, which means their own contributions to the combined work must also be open-sourced. This limits their commercial options for proprietary development.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_developers_seeking_proprietary_derivatives, payer,
    moderate, biographical, constrained, global).

% Non-copyleft open-source licenses (e.g., MIT, Apache) offer more permissive terms, allowing proprietary integration. While they coexist, the GPL's strong reciprocity obligation suppresses the market for proprietary derivatives that might otherwise use these more permissive licenses, effectively excluding them from certain integration paths.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, alternative_licensing_schemes, excluded,
    organized, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of software by ensuring that all derivative works remain free and open, fostering a collaborative ecosystem where contributions benefit all users.
% TRANSFER_FUNCTION: Transfers the obligation to share source code and modifications from original authors to anyone who distributes a derivative work, effectively transferring 'freedom' to downstream users and the community, and restricting proprietary capture.
% ABSENT_VOICES: Proprietary software companies and developers who prioritize closed-source business models are structurally excluded from integrating GPL-licensed code into proprietary products without significant concessions. They would argue for greater flexibility and less 'viral' terms.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, a significant portion of currently free software would likely be integrated into proprietary products without its source code being released. This would lead to a fragmentation of the open-source ecosystem, reduced user freedoms, and a shift in power towards proprietary vendors, fundamentally altering the software landscape.
% FOUNDING_PROBLEM: The problem of software becoming proprietary and users losing the freedom to study, modify, and share it, leading to vendor lock-in and reduced innovation in the public domain.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and many open-source advocates continually attest to the ongoing threat of proprietary capture. Independent legal scholars and technologists corroborate that without strong copyleft, proprietary interests would likely enclose more software, diminishing user freedoms.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because proprietary integrators are compelled to forgo potential exclusive monetization of their contributions. Suppression is also high (0.78) due to the legal enforceability of the license and the community's active defense against violations, which effectively suppresses alternative licensing choices for derivative works. Theater ratio is low (0.1) as the license's function is direct and actively maintained, not performative. The metrics reflect the active, coercive aspect of copyleft from the perspective of those whose 'freedom to enclose' is restricted.
 *
 * PERSPECTIVAL GAP:
 *   The 'copyleft as freedom' reading emphasizes the benefits to users and the free software ecosystem, while the 'copyleft as restriction' reading (a sibling constraint) highlights the limitations imposed on proprietary developers. The engine's per-seat classification will reflect this divergence: a beneficiary for users, a target for proprietary integrators.
 *
 * DIRECTIONALITY LOGIC:
 *   The Free Software Community and downstream users are the primary beneficiaries, gaining guaranteed access to free software and protection from proprietary lock-in. Proprietary integrators and commercial developers seeking proprietary derivatives are the victims/payers, as they bear the cost of the reciprocity obligation. Alternative licensing schemes are excluded, as the GPL's dominance in certain ecosystems limits their practical application for proprietary integration.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_definition_ambiguity,
    'Is ''freedom'' primarily defined as the user''s right to modify and share software, or as a developer''s right to choose any licensing model, including proprietary ones?',
    'Conceptual analysis of philosophical underpinnings of ''freedom'' in software, and empirical study of user vs. developer preferences and outcomes.',
    'If developer freedom to choose proprietary models is prioritized, the GPL''s reciprocity would be reclassified as higher extraction; if user freedom is prioritized, the current classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''freedom'' in software licensing.').

omega_variable(
    enforcement_sustainability,
    'Is the active enforcement of the GPL''s reciprocity obligation sustainable in the long term against well-resourced proprietary interests?',
    'Longitudinal study of GPL enforcement actions, legal challenges, and community funding for compliance efforts.',
    'If enforcement becomes unsustainable, the constraint''s effective suppression would decrease, potentially leading to a reclassification towards Piton as its function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Sustainability of GPL enforcement mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(gpl__tr_t40, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(gpl__be_t40, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(gpl__su_t40, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'gpl_reciprocity_obligation' kernel. This 'copyleft as freedom' reading emphasizes user rights and anti-capture, while sibling readings focus on restriction or commons management. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
